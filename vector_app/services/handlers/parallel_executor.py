"""
Parallel Step Executor

Executes independent plan steps in parallel using ThreadPoolExecutor.
Analyzes step dependencies based on file outputs and groups steps for concurrent execution.
"""
import logging
import time
import queue
from concurrent.futures import ThreadPoolExecutor, Future
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, Generator, List, Optional, Set, Tuple, TYPE_CHECKING

from .base_handler import AgentEvent, FileChange, PlanStep

if TYPE_CHECKING:
    from vector_app.models import InternalApp, AppVersion

logger = logging.getLogger(__name__)


@dataclass
class StepResult:
    """Result of a step execution."""
    step_index: int
    step: PlanStep
    files: List[FileChange]
    events: List[AgentEvent]
    error: Optional[Exception] = None
    duration_ms: int = 0


@dataclass
class StepDependencyInfo:
    """Dependency information for a step."""
    step_index: int
    step: PlanStep
    # Files this step is likely to produce (based on step type and description)
    expected_outputs: Set[str] = field(default_factory=set)
    # Files this step depends on (from existing_files at execution time)
    depends_on: Set[str] = field(default_factory=set)
    # Step indices this step depends on
    dependent_on_steps: Set[int] = field(default_factory=set)


class StepDependencyAnalyzer:
    """
    Analyzes dependencies between plan steps to determine which can run in parallel.
    
    Uses heuristics based on step types and descriptions to infer file dependencies.
    """
    
    # Step types that typically produce specific output patterns
    STEP_OUTPUT_PATTERNS = {
        'design': {'src/App.tsx', 'src/types.ts', 'src/types/'},
        'component': {'src/components/'},
        'styling': {'src/styles/', 'src/index.css'},
        'integration': {'src/hooks/', 'src/services/', 'src/api/'},
        'validation': set(),  # Validation doesn't produce files
        'code': {'src/'},  # Generic code can produce anything
    }
    
    # Step types that typically depend on certain outputs
    STEP_DEPENDENCIES = {
        'component': {'design'},  # Components depend on design
        'integration': {'component', 'design'},  # Integration depends on components
        'styling': {'component', 'design'},  # Styling can depend on components
        'validation': {'component', 'integration', 'styling'},  # Validation depends on all
    }
    
    def analyze(self, steps: List[PlanStep]) -> List[List[int]]:
        """
        Analyze step dependencies and return execution waves.
        
        Each wave contains step indices that can be executed in parallel.
        Steps in later waves depend on steps in earlier waves.
        
        Returns:
            List of waves, where each wave is a list of step indices.
        """
        if not steps:
            return []
        
        # Build dependency info for each step
        dep_info = [self._build_dependency_info(i, step, steps) for i, step in enumerate(steps)]
        
        # Group into waves
        waves = self._build_execution_waves(dep_info)
        
        logger.info(f"Analyzed {len(steps)} steps into {len(waves)} execution waves")
        for i, wave in enumerate(waves):
            wave_steps = [steps[idx].title for idx in wave]
            logger.debug(f"Wave {i}: {wave_steps}")
        
        return waves
    
    def _build_dependency_info(
        self,
        step_index: int,
        step: PlanStep,
        all_steps: List[PlanStep],
    ) -> StepDependencyInfo:
        """Build dependency information for a single step."""
        info = StepDependencyInfo(
            step_index=step_index,
            step=step,
        )
        
        # Determine expected outputs based on step type
        step_type = step.type.lower()
        if step_type in self.STEP_OUTPUT_PATTERNS:
            info.expected_outputs = self.STEP_OUTPUT_PATTERNS[step_type].copy()
        
        # Analyze step description for hints about file outputs
        description_lower = step.description.lower()
        title_lower = step.title.lower()
        
        # Check for component-specific outputs
        if 'component' in description_lower or 'component' in title_lower:
            info.expected_outputs.add('src/components/')
        
        # Check for App.tsx modifications
        if 'app' in description_lower or 'main' in description_lower:
            info.expected_outputs.add('src/App.tsx')
        
        # Determine dependencies based on step type
        if step_type in self.STEP_DEPENDENCIES:
            required_types = self.STEP_DEPENDENCIES[step_type]
            for i, other_step in enumerate(all_steps):
                if i >= step_index:
                    continue
                if other_step.type.lower() in required_types:
                    info.dependent_on_steps.add(i)
        
        # Always depend on design step if it exists and we're not design
        if step_type != 'design':
            for i, other_step in enumerate(all_steps):
                if i >= step_index:
                    continue
                if other_step.type.lower() == 'design':
                    info.dependent_on_steps.add(i)
                    break
        
        return info
    
    def _build_execution_waves(
        self,
        dep_info_list: List[StepDependencyInfo],
    ) -> List[List[int]]:
        """
        Build execution waves from dependency information.
        
        Uses a topological sort approach, grouping steps that have all
        their dependencies satisfied in the same wave.
        """
        waves: List[List[int]] = []
        completed: Set[int] = set()
        remaining: Set[int] = set(range(len(dep_info_list)))
        
        while remaining:
            # Find all steps whose dependencies are satisfied
            current_wave = []
            for idx in remaining:
                info = dep_info_list[idx]
                if info.dependent_on_steps.issubset(completed):
                    current_wave.append(idx)
            
            if not current_wave:
                # Circular dependency or error - just add remaining sequentially
                logger.warning("Could not resolve step dependencies, executing remaining steps sequentially")
                for idx in sorted(remaining):
                    waves.append([idx])
                break
            
            # Sort by original index to maintain some ordering preference
            current_wave.sort()
            waves.append(current_wave)
            
            # Mark as completed
            completed.update(current_wave)
            remaining -= set(current_wave)
        
        return waves


class ParallelStepExecutor:
    """
    Executes plan steps in parallel when dependencies allow.
    
    Uses ThreadPoolExecutor for concurrent step execution and maintains
    event ordering for streaming to the frontend.
    """
    
    def __init__(self, max_workers: int = 3):
        """
        Initialize the parallel executor.
        
        Args:
            max_workers: Maximum number of concurrent step executions.
                        Limited to prevent overwhelming the LLM API.
        """
        self.max_workers = max_workers
        self.analyzer = StepDependencyAnalyzer()
    
    def execute_steps(
        self,
        steps: List[PlanStep],
        step_executor: Callable[[PlanStep, int, List[FileChange]], Generator[AgentEvent, None, None]],
        initial_files: Optional[List[FileChange]] = None,
    ) -> Generator[AgentEvent, None, List[FileChange]]:
        """
        Execute plan steps with parallelization for independent steps.
        
        Args:
            steps: List of plan steps to execute
            step_executor: Function that executes a single step, signature:
                          (step, step_index, existing_files) -> Generator[AgentEvent]
                          The generator should also append files to existing_files
            initial_files: Initial list of files (will be extended)
            
        Yields:
            AgentEvent objects in order
            
        Returns:
            List of all generated files
        """
        if initial_files is None:
            initial_files = []
        
        generated_files = list(initial_files)
        
        # Analyze dependencies and get execution waves
        waves = self.analyzer.analyze(steps)
        
        # If only single-step waves, execute sequentially (no benefit from parallelization)
        if all(len(wave) == 1 for wave in waves):
            logger.info("All steps are sequential, executing without parallelization")
            yield from self._execute_sequential(steps, step_executor, generated_files)
            return generated_files
        
        # Execute each wave
        for wave_idx, wave in enumerate(waves):
            if len(wave) == 1:
                # Single step - execute directly without threading overhead
                step_idx = wave[0]
                step = steps[step_idx]
                
                yield from self._execute_single_step(
                    step, step_idx, step_executor, generated_files
                )
            else:
                # Multiple steps - execute in parallel
                logger.info(f"Executing wave {wave_idx} with {len(wave)} parallel steps")
                
                yield from self._execute_wave_parallel(
                    wave, steps, step_executor, generated_files
                )
        
        return generated_files
    
    def _execute_sequential(
        self,
        steps: List[PlanStep],
        step_executor: Callable,
        generated_files: List[FileChange],
    ) -> Generator[AgentEvent, None, None]:
        """Execute all steps sequentially."""
        for i, step in enumerate(steps):
            yield from self._execute_single_step(step, i, step_executor, generated_files)
    
    def _execute_single_step(
        self,
        step: PlanStep,
        step_index: int,
        step_executor: Callable,
        generated_files: List[FileChange],
    ) -> Generator[AgentEvent, None, None]:
        """Execute a single step and yield its events."""
        step_start = time.time()
        
        try:
            # Execute the step
            for event in step_executor(step, step_index, generated_files):
                yield event
                # Track file generation events
                if event.type == "file_generated":
                    file_data = event.data.get("file", {})
                    generated_files.append(FileChange(
                        path=file_data.get("path", ""),
                        action=file_data.get("action", "create"),
                        language=file_data.get("language", "tsx"),
                        content=file_data.get("content", ""),
                    ))
            
            step.status = "complete"
            step.duration = int((time.time() - step_start) * 1000)
            
        except Exception as e:
            logger.error(f"Step {step_index} execution error: {e}")
            step.status = "error"
            step.duration = int((time.time() - step_start) * 1000)
            raise
    
    def _execute_wave_parallel(
        self,
        wave: List[int],
        steps: List[PlanStep],
        step_executor: Callable,
        generated_files: List[FileChange],
    ) -> Generator[AgentEvent, None, None]:
        """
        Execute a wave of steps in parallel.
        
        Events are yielded in real-time as they arrive from parallel steps,
        allowing the UI to show true parallel progress. Step start events are
        emitted immediately so all parallel steps show as "in progress".
        """
        # Thread-safe queue for events from parallel threads
        # Events are tuples: (timestamp, step_index, event_or_none)
        # None signals step completion
        event_queue: queue.Queue = queue.Queue()
        
        # Snapshot of current files for all parallel steps to use
        files_snapshot = list(generated_files)
        
        def execute_step_thread(step_idx: int) -> StepResult:
            """Execute a step in a thread and stream events to the queue."""
            step = steps[step_idx]
            step_start = time.time()
            events: List[AgentEvent] = []
            files: List[FileChange] = []
            error: Optional[Exception] = None
            
            try:
                # Use a copy of files for this step's context
                step_files = list(files_snapshot)
                
                for event in step_executor(step, step_idx, step_files):
                    events.append(event)
                    
                    # Stream event immediately to the queue with timestamp
                    # This allows real-time interleaved output
                    event_queue.put((time.time(), step_idx, event))
                    
                    # Track generated files
                    if event.type == "file_generated":
                        file_data = event.data.get("file", {})
                        files.append(FileChange(
                            path=file_data.get("path", ""),
                            action=file_data.get("action", "create"),
                            language=file_data.get("language", "tsx"),
                            content=file_data.get("content", ""),
                        ))
                
                step.status = "complete"
                step.duration = int((time.time() - step_start) * 1000)
                
            except Exception as e:
                logger.error(f"Parallel step {step_idx} error: {e}")
                step.status = "error"
                step.duration = int((time.time() - step_start) * 1000)
                error = e
            
            # Signal completion with None event
            event_queue.put((time.time(), step_idx, None))
            
            return StepResult(
                step_index=step_idx,
                step=step,
                files=files,
                events=events,
                error=error,
                duration_ms=step.duration or 0,
            )
        
        logger.info(f"Starting parallel execution of {len(wave)} steps: {[steps[i].title for i in wave]}")
        
        # Execute all steps in the wave in parallel
        with ThreadPoolExecutor(max_workers=min(self.max_workers, len(wave))) as executor:
            futures: Dict[int, Future] = {}
            for step_idx in wave:
                futures[step_idx] = executor.submit(execute_step_thread, step_idx)
            
            # Stream events as they arrive - true interleaved parallel output
            completed_steps: Set[int] = set()
            
            while len(completed_steps) < len(wave):
                try:
                    # Get next event with short timeout
                    timestamp, step_idx, event = event_queue.get(timeout=0.05)
                    
                    if event is None:
                        # Step completed
                        completed_steps.add(step_idx)
                        logger.debug(f"Step {step_idx} ({steps[step_idx].title}) completed")
                        continue
                    
                    # Yield event immediately - this gives true parallel progress in UI
                    yield event
                    
                except queue.Empty:
                    # No events ready, continue polling
                    continue
            
            # Collect results and add files to generated_files
            first_error = None
            for step_idx in wave:
                result = futures[step_idx].result()
                generated_files.extend(result.files)
                
                # Track first error to raise after collecting all results
                if result.error and first_error is None:
                    first_error = result.error
            
            # Raise first error if any step failed
            if first_error:
                raise first_error
        
        logger.info(f"Completed parallel wave: {[steps[i].title for i in wave]}")


def create_parallel_executor(max_workers: int = 3) -> ParallelStepExecutor:
    """Create a parallel step executor instance."""
    return ParallelStepExecutor(max_workers=max_workers)

