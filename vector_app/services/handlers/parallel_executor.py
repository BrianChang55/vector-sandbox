"""
Parallel Step Executor

Executes independent plan steps in parallel using ThreadPoolExecutor.
Groups steps by step_order for wave-based parallel execution.
"""
import logging
import time
import queue
from concurrent.futures import ThreadPoolExecutor, Future
from dataclasses import dataclass
from itertools import groupby
from typing import Callable, Dict, Generator, List, Optional, Set, TYPE_CHECKING

from .base_handler import AgentEvent, FileChange
from vector_app.services.planning_service import PlanStep, PlanStepStatus

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


class ParallelStepExecutor:
    """
    Executes plan steps in parallel when dependencies allow.
    
    Uses ThreadPoolExecutor for concurrent step execution and maintains
    event ordering for streaming to the frontend.
    
    Steps are grouped into waves by their step_order field. Steps with
    the same step_order execute in parallel.
    """
    
    def __init__(self, max_workers: int = 5):
        """
        Initialize the parallel executor.
        
        Args:
            max_workers: Maximum number of concurrent step executions.
                        Limited to prevent overwhelming the LLM API.
        """
        self.max_workers = max_workers
    
    def analyze_by_step_order(self, steps: List[PlanStep]) -> List[List[int]]:
        """
        Group steps into execution waves by their step_order field.
        
        Steps with the same step_order will be placed in the same wave
        and can execute in parallel.
        
        Returns:
            List of waves, where each wave is a list of step indices.
        """
        if not steps:
            return []
        
        # Sort indices by step_order, then group by step_order
        indexed_steps = sorted(enumerate(steps), key=lambda x: x[1].step_order)
        waves = [
            [idx for idx, _ in group]
            for _, group in groupby(indexed_steps, key=lambda x: x[1].step_order)
        ]
        
        logger.info(f"Grouped {len(steps)} steps into {len(waves)} execution waves by step_order")
        for i, wave in enumerate(waves):
            wave_steps = [steps[idx].title for idx in wave]
            step_order = steps[wave[0]].step_order if wave else 0
            logger.debug(f"Wave {i} (step_order={step_order}): {wave_steps}")
        
        return waves
    
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
        
        # Group steps into waves by step_order
        waves = self.analyze_by_step_order(steps)
        
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
                        previous_content=file_data.get("previous_content", ""),
                        lines_added=file_data.get("lines_added", 0),
                        lines_removed=file_data.get("lines_removed", 0),
                    ))
            
            step.status = PlanStepStatus.COMPLETE
            step.duration = int((time.time() - step_start) * 1000)
            
        except Exception as e:
            logger.error(f"Step {step_index} execution error: {e}")
            step.status = PlanStepStatus.ERROR
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
                            previous_content=file_data.get("previous_content", ""),
                            lines_added=file_data.get("lines_added", 0),
                            lines_removed=file_data.get("lines_removed", 0),
                        ))
                
                step.status = PlanStepStatus.COMPLETE
                step.duration = int((time.time() - step_start) * 1000)
                
            except Exception as e:
                logger.error(f"Parallel step {step_idx} error: {e}")
                step.status = PlanStepStatus.ERROR
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


def create_parallel_executor(max_workers: int = 5) -> ParallelStepExecutor:
    """Create a parallel step executor instance."""
    return ParallelStepExecutor(max_workers=max_workers)

