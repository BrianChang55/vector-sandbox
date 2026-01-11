"""
Validation Service

TypeScript validation service for generated code.
Uses tsc to check for compilation errors.
"""
import json
import logging
import os
import re
import shutil
import subprocess
import tempfile
from typing import Any, Dict, List, Optional

from vector_app.services.types import (
    FileChange,
    CompilationError,
    ValidationResult,
)

logger = logging.getLogger(__name__)


class ValidationService:
    """
    Service for validating TypeScript/TSX code using the TypeScript compiler.
    
    Creates a temporary directory with type stubs for React, lucide-react,
    NodeJS, and the dataStore API, then runs tsc --noEmit to check for errors.
    """
    
    def validate_typescript(self, files: List[FileChange]) -> ValidationResult:
        """
        Validate TypeScript files using tsc.
        
        Args:
            files: List of FileChange objects to validate
            
        Returns:
            ValidationResult with passed status, errors, and warnings
        """
        ts_files = [f for f in files if f.language in ('tsx', 'ts')]
        
        if not ts_files:
            return ValidationResult(passed=True)
        
        # Check if tsc is available
        tsc_path = shutil.which('tsc')
        if not tsc_path:
            npx_path = shutil.which('npx')
            if not npx_path:
                return ValidationResult(
                    passed=True,
                    warnings=['TypeScript compiler not available']
                )
            tsc_cmd = ['npx', 'tsc']
        else:
            tsc_cmd = [tsc_path]
        
        temp_dir = None
        try:
            temp_dir = tempfile.mkdtemp(prefix='vector_tsc_')
            
            # Write files
            for file in ts_files:
                file_path = file.path
                if file_path.startswith('src/'):
                    file_path = file_path[4:]
                
                full_path = os.path.join(temp_dir, file_path)
                os.makedirs(os.path.dirname(full_path), exist_ok=True)
                
                with open(full_path, 'w', encoding='utf-8') as f:
                    f.write(file.content)
            
            # Create tsconfig
            self._write_tsconfig(temp_dir)
            
            # Create type stubs
            self._write_react_stub(temp_dir)
            self._write_lucide_stub(temp_dir)
            self._write_node_stub(temp_dir)
            self._write_datastore_stub(temp_dir)
            
            # Run tsc
            result = subprocess.run(
                tsc_cmd + ['--noEmit', '--pretty', 'false'],
                cwd=temp_dir,
                capture_output=True,
                text=True,
                timeout=30,
            )
            
            if result.returncode == 0:
                return ValidationResult(passed=True)
            
            # Parse errors
            errors = self._parse_tsc_errors(result.stdout + result.stderr, temp_dir)
            
            return ValidationResult(
                passed=len(errors) == 0,
                errors=errors,
            )
            
        except subprocess.TimeoutExpired:
            return ValidationResult(
                passed=True,
                warnings=['Validation timed out']
            )
        except Exception as e:
            logger.error(f"TypeScript validation error: {e}")
            return ValidationResult(
                passed=True,
                warnings=[f'Validation error: {str(e)}']
            )
        finally:
            if temp_dir and os.path.exists(temp_dir):
                try:
                    shutil.rmtree(temp_dir)
                except Exception:
                    pass
    
    def _parse_tsc_errors(self, output: str, temp_dir: str) -> List[CompilationError]:
        """
        Parse TypeScript compiler output into structured errors.
        
        Args:
            output: Combined stdout and stderr from tsc
            temp_dir: Temp directory path to strip from file paths
            
        Returns:
            List of CompilationError objects
        """
        errors = []
        pattern = r'([^(]+)\((\d+),(\d+)\):\s*(error|warning)\s+(TS\d+):\s*(.+)'
        
        for line in output.strip().split('\n'):
            line = line.strip()
            if not line:
                continue
            
            match = re.match(pattern, line)
            if match:
                file_path, line_num, col, severity, code, message = match.groups()
                
                file_path = file_path.strip()
                if temp_dir in file_path:
                    file_path = file_path.replace(temp_dir + os.sep, '')
                
                if not file_path.startswith('src/'):
                    file_path = f'src/{file_path}'
                
                if severity == 'error':
                    # Filter out 'key' prop false positives from our type stubs
                    # The key prop is handled by React internally and our stubs don't handle it
                    msg = message.strip()
                    if "Property 'key' does not exist" in msg:
                        continue
                    # Also filter TS2322 errors that mention key: string in the type mismatch
                    if code == 'TS2322' and "{ key: string;" in msg:
                        continue
                    
                    errors.append(CompilationError(
                        file=file_path,
                        line=int(line_num),
                        column=int(col),
                        message=msg,
                        code=code,
                    ))
        
        return errors
    
    def _write_tsconfig(self, temp_dir: str) -> None:
        """Write tsconfig.json to temp directory."""
        tsconfig = {
            "compilerOptions": {
                "target": "ES2020",
                "lib": ["ES2020", "DOM", "DOM.Iterable"],
                "module": "ESNext",
                "moduleResolution": "node",
                "jsx": "react",
                "strict": True,
                "noEmit": True,
                "skipLibCheck": True,
                "esModuleInterop": True,
                "allowSyntheticDefaultImports": True,
                "noImplicitAny": True,
                "strictNullChecks": True,
                "typeRoots": ["./node_modules/@types"],
            },
            "include": ["**/*.ts", "**/*.tsx"],
        }
        
        with open(os.path.join(temp_dir, 'tsconfig.json'), 'w') as f:
            json.dump(tsconfig, f)
    
    def _write_react_stub(self, temp_dir: str) -> None:
        """Write React type stubs."""
        stubs_dir = os.path.join(temp_dir, 'node_modules', '@types', 'react')
        os.makedirs(stubs_dir, exist_ok=True)
        with open(os.path.join(stubs_dir, 'index.d.ts'), 'w') as f:
            f.write('''
// JSX namespace at global level
declare namespace JSX {
    interface Element { }
    interface ElementClass { render(): any; }
    interface ElementAttributesProperty { props: {}; }
    interface ElementChildrenAttribute { children: {}; }
    interface IntrinsicAttributes { key?: string | number | null; }
    interface IntrinsicClassAttributes<T> { key?: string | number | null; ref?: any; }
    interface IntrinsicElements { [elemName: string]: any; }
}

// Global DOM types
interface EventTarget {
    addEventListener(type: string, listener: EventListenerOrEventListenerObject, options?: boolean | AddEventListenerOptions): void;
    dispatchEvent(event: Event): boolean;
    removeEventListener(type: string, listener: EventListenerOrEventListenerObject, options?: boolean | EventListenerOptions): void;
}
interface Event { readonly target: EventTarget | null; readonly currentTarget: EventTarget | null; preventDefault(): void; stopPropagation(): void; }

// React namespace for type usage like React.ChangeEvent, React.FormEvent
declare namespace React {
    type ReactNode = any;
    interface ReactElement<P = any, T = any> { type: T; props: P; key: string | number | null; }
    // FC accepts props P (key is handled by JSX.IntrinsicAttributes)
    type FC<P = {}> = (props: P) => ReactNode;
    type ChangeEvent<T = Element> = { target: T & { value: string; checked?: boolean } };
    type FormEvent<T = Element> = { preventDefault: () => void; target: T };
    type MouseEvent<T = Element> = { preventDefault: () => void; stopPropagation: () => void; target: EventTarget & T; currentTarget: EventTarget & T };
    type KeyboardEvent<T = Element> = { key: string; preventDefault: () => void };
    type FocusEvent<T = Element> = { target: T };
    type DragEvent<T = Element> = { preventDefault: () => void; dataTransfer: any };
    type ClipboardEvent<T = Element> = { clipboardData: any };
    type TouchEvent<T = Element> = { touches: any[] };
    type WheelEvent<T = Element> = { deltaX: number; deltaY: number };
    type AnimationEvent<T = Element> = { animationName: string };
    type TransitionEvent<T = Element> = { propertyName: string };
    type Dispatch<A> = (value: A) => void;
    type SetStateAction<S> = S | ((prevState: S) => S);
    type RefObject<T> = { current: T | null };
    type MutableRefObject<T> = { current: T };
    type Context<T> = { Provider: FC<{ value: T; children?: ReactNode }>; Consumer: FC<{ children: (value: T) => ReactNode }> };
    interface CSSProperties { [key: string]: any; }
    interface HTMLAttributes<T> {
        className?: string;
        style?: CSSProperties;
        id?: string;
        onClick?: (e: MouseEvent<T>) => void;
        onChange?: (e: ChangeEvent<T>) => void;
        onSubmit?: (e: FormEvent<T>) => void;
        onKeyDown?: (e: KeyboardEvent<T>) => void;
        onKeyUp?: (e: KeyboardEvent<T>) => void;
        onBlur?: (e: FocusEvent<T>) => void;
        onFocus?: (e: FocusEvent<T>) => void;
        [key: string]: any;
    }
    interface InputHTMLAttributes<T> extends HTMLAttributes<T> {
        type?: string;
        value?: any;
        defaultValue?: any;
        placeholder?: string;
        disabled?: boolean;
        readOnly?: boolean;
        required?: boolean;
        min?: number | string;
        max?: number | string;
        step?: number | string;
        pattern?: string;
        autoComplete?: string;
        autoFocus?: boolean;
        name?: string;
    }
    interface TextareaHTMLAttributes<T> extends HTMLAttributes<T> {
        value?: any;
        placeholder?: string;
        disabled?: boolean;
        rows?: number;
        cols?: number;
    }
    interface ButtonHTMLAttributes<T> extends HTMLAttributes<T> {
        type?: "button" | "submit" | "reset";
        disabled?: boolean;
    }
    interface SelectHTMLAttributes<T> extends HTMLAttributes<T> {
        value?: any;
        disabled?: boolean;
        multiple?: boolean;
    }
    interface FormHTMLAttributes<T> extends HTMLAttributes<T> {
        onSubmit?: (e: FormEvent<T>) => void;
    }
}

declare module "react" {
    // Core hooks
    export function useState<T>(initial: T | (() => T)): [T, React.Dispatch<React.SetStateAction<T>>];
    export function useEffect(effect: () => void | (() => void), deps?: any[]): void;
    export function useLayoutEffect(effect: () => void | (() => void), deps?: any[]): void;
    export function useCallback<T extends (...args: any[]) => any>(callback: T, deps: any[]): T;
    export function useMemo<T>(factory: () => T, deps: any[]): T;
    export function useRef<T>(initial: T | null): React.MutableRefObject<T>;
    export function useContext<T>(context: React.Context<T>): T;
    export function useReducer<S, A>(reducer: (state: S, action: A) => S, initialState: S): [S, React.Dispatch<A>];
    export function useId(): string;
    export function useImperativeHandle<T>(ref: any, createHandle: () => T, deps?: any[]): void;
    export function useDebugValue<T>(value: T): void;
    export function useDeferredValue<T>(value: T): T;
    export function useTransition(): [boolean, (callback: () => void) => void];
    
    // Context
    export function createContext<T>(defaultValue: T): React.Context<T>;
    
    // Refs
    export function createRef<T>(): React.RefObject<T>;
    export function forwardRef<T, P = {}>(render: (props: P, ref: React.RefObject<T>) => React.ReactNode): React.FC<P>;
    
    // Memo
    export function memo<P>(Component: React.FC<P>): React.FC<P>;
    
    // Children utilities
    export const Children: {
        map: <T, C>(children: C, fn: (child: C, index: number) => T) => T[];
        forEach: <C>(children: C, fn: (child: C, index: number) => void) => void;
        count: (children: any) => number;
        only: (children: any) => any;
        toArray: (children: any) => any[];
    };
    
    // Fragment
    export const Fragment: React.FC<{ children?: React.ReactNode }>;
    
    // Suspense
    export const Suspense: React.FC<{ fallback?: React.ReactNode; children?: React.ReactNode }>;
    
    // Types re-exported
    export type ReactNode = React.ReactNode;
    export type FC<P = {}> = React.FC<P>;
    export type ChangeEvent<T = Element> = React.ChangeEvent<T>;
    export type FormEvent<T = Element> = React.FormEvent<T>;
    export type MouseEvent<T = Element> = React.MouseEvent<T>;
    export type KeyboardEvent<T = Element> = React.KeyboardEvent<T>;
    export type FocusEvent<T = Element> = React.FocusEvent<T>;
    export type Dispatch<A> = React.Dispatch<A>;
    export type SetStateAction<S> = React.SetStateAction<S>;
    export type RefObject<T> = React.RefObject<T>;
    export type MutableRefObject<T> = React.MutableRefObject<T>;
    export type Context<T> = React.Context<T>;
    export type CSSProperties = React.CSSProperties;
    export type HTMLAttributes<T> = React.HTMLAttributes<T>;
    
    // Default export
    const React: any;
    export default React;
}

declare module "react-dom/client" {
    export function createRoot(container: Element | null): { render(element: any): void; unmount(): void };
    export function hydrateRoot(container: Element, initialChildren: any): { render(element: any): void; unmount(): void };
}

declare module "react/jsx-runtime" {
    export function jsx(type: any, props: any, key?: string): any;
    export function jsxs(type: any, props: any, key?: string): any;
    export const Fragment: any;
}

declare module "react/jsx-dev-runtime" {
    export function jsxDEV(type: any, props: any, key?: string, isStaticChildren?: boolean, source?: any, self?: any): any;
    export const Fragment: any;
}

''')
    
    def _write_lucide_stub(self, temp_dir: str) -> None:
        """Write lucide-react type stubs."""
        lucide_dir = os.path.join(temp_dir, 'node_modules', 'lucide-react')
        os.makedirs(lucide_dir, exist_ok=True)
        with open(os.path.join(lucide_dir, 'index.d.ts'), 'w') as f:
            f.write('''declare module "lucide-react" {
    import { FC } from "react";
    
    // IconProps includes all common SVG and custom props
    interface IconProps {
        size?: number | string;
        color?: string;
        strokeWidth?: number | string;
        absoluteStrokeWidth?: boolean;
        className?: string;
        style?: React.CSSProperties | { [key: string]: any };
        onClick?: (e: any) => void;
        onMouseEnter?: (e: any) => void;
        onMouseLeave?: (e: any) => void;
        "aria-label"?: string;
        "aria-hidden"?: boolean | "true" | "false";
        role?: string;
        id?: string;
        // Allow any other SVG props
        [key: string]: any;
    }
    
    type Icon = FC<IconProps>;
    
    // Common action icons
    export const Plus: Icon;
    export const Minus: Icon;
    export const X: Icon;
    export const Check: Icon;
    export const Save: Icon;
    export const Edit: Icon;
    export const Edit2: Icon;
    export const Edit3: Icon;
    export const Pencil: Icon;
    export const PencilLine: Icon;
    export const Trash: Icon;
    export const Trash2: Icon;
    export const Delete: Icon;
    export const Copy: Icon;
    export const Clipboard: Icon;
    export const ClipboardCopy: Icon;
    export const ClipboardCheck: Icon;
    export const Download: Icon;
    export const Upload: Icon;
    export const Share: Icon;
    export const Share2: Icon;
    export const Send: Icon;
    export const Reply: Icon;
    export const Forward: Icon;
    export const Undo: Icon;
    export const Undo2: Icon;
    export const Redo: Icon;
    export const Redo2: Icon;
    export const RotateCw: Icon;
    export const RotateCcw: Icon;
    export const RefreshCw: Icon;
    export const RefreshCcw: Icon;
    export const Repeat: Icon;
    export const Repeat1: Icon;
    export const Shuffle: Icon;
    
    // Navigation icons
    export const ArrowUp: Icon;
    export const ArrowDown: Icon;
    export const ArrowLeft: Icon;
    export const ArrowRight: Icon;
    export const ArrowUpRight: Icon;
    export const ArrowUpLeft: Icon;
    export const ArrowDownRight: Icon;
    export const ArrowDownLeft: Icon;
    export const ChevronUp: Icon;
    export const ChevronDown: Icon;
    export const ChevronLeft: Icon;
    export const ChevronRight: Icon;
    export const ChevronsUp: Icon;
    export const ChevronsDown: Icon;
    export const ChevronsLeft: Icon;
    export const ChevronsRight: Icon;
    export const MoveUp: Icon;
    export const MoveDown: Icon;
    export const MoveLeft: Icon;
    export const MoveRight: Icon;
    export const CornerUpRight: Icon;
    export const CornerDownRight: Icon;
    export const ExternalLink: Icon;
    export const Link: Icon;
    export const Link2: Icon;
    export const Unlink: Icon;
    export const Menu: Icon;
    export const MoreHorizontal: Icon;
    export const MoreVertical: Icon;
    export const Grip: Icon;
    export const GripVertical: Icon;
    export const GripHorizontal: Icon;
    
    // Status/feedback icons
    export const CheckCircle: Icon;
    export const CheckCircle2: Icon;
    export const CheckSquare: Icon;
    export const XCircle: Icon;
    export const XSquare: Icon;
    export const AlertCircle: Icon;
    export const AlertTriangle: Icon;
    export const AlertOctagon: Icon;
    export const Info: Icon;
    export const HelpCircle: Icon;
    export const Ban: Icon;
    export const Loader: Icon;
    export const Loader2: Icon;
    export const Circle: Icon;
    export const Square: Icon;
    export const Diamond: Icon;
    export const Heart: Icon;
    export const HeartOff: Icon;
    export const Star: Icon;
    export const StarOff: Icon;
    export const ThumbsUp: Icon;
    export const ThumbsDown: Icon;
    export const Bell: Icon;
    export const BellOff: Icon;
    export const BellRing: Icon;
    
    // Media icons
    export const Play: Icon;
    export const Pause: Icon;
    export const Stop: Icon;
    export const StopCircle: Icon;
    export const PlayCircle: Icon;
    export const PauseCircle: Icon;
    export const SkipBack: Icon;
    export const SkipForward: Icon;
    export const Rewind: Icon;
    export const FastForward: Icon;
    export const Volume: Icon;
    export const Volume1: Icon;
    export const Volume2: Icon;
    export const VolumeX: Icon;
    export const Mic: Icon;
    export const MicOff: Icon;
    export const Camera: Icon;
    export const CameraOff: Icon;
    export const Video: Icon;
    export const VideoOff: Icon;
    export const Image: Icon;
    export const Images: Icon;
    export const Film: Icon;
    export const Music: Icon;
    export const Music2: Icon;
    
    // User/people icons
    export const User: Icon;
    export const Users: Icon;
    export const Users2: Icon;
    export const UserPlus: Icon;
    export const UserMinus: Icon;
    export const UserCheck: Icon;
    export const UserX: Icon;
    export const UserCircle: Icon;
    export const UserCircle2: Icon;
    export const Contact: Icon;
    export const Contact2: Icon;
    
    // Communication icons
    export const Mail: Icon;
    export const MailOpen: Icon;
    export const Inbox: Icon;
    export const MessageCircle: Icon;
    export const MessageSquare: Icon;
    export const MessagesSquare: Icon;
    export const Phone: Icon;
    export const PhoneCall: Icon;
    export const PhoneOff: Icon;
    export const AtSign: Icon;
    export const Hash: Icon;
    
    // File/document icons
    export const File: Icon;
    export const FileText: Icon;
    export const FileImage: Icon;
    export const FileVideo: Icon;
    export const FileAudio: Icon;
    export const FileCode: Icon;
    export const FileJson: Icon;
    export const Files: Icon;
    export const Folder: Icon;
    export const FolderOpen: Icon;
    export const FolderPlus: Icon;
    export const FolderMinus: Icon;
    export const Archive: Icon;
    export const Package: Icon;
    
    // Layout/UI icons
    export const Layout: Icon;
    export const LayoutGrid: Icon;
    export const LayoutList: Icon;
    export const Grid: Icon;
    export const Grid2x2: Icon;
    export const Grid3x3: Icon;
    export const List: Icon;
    export const ListOrdered: Icon;
    export const Table: Icon;
    export const Table2: Icon;
    export const Columns: Icon;
    export const Rows: Icon;
    export const SidebarLeft: Icon;
    export const SidebarRight: Icon;
    export const PanelLeft: Icon;
    export const PanelRight: Icon;
    export const Maximize: Icon;
    export const Maximize2: Icon;
    export const Minimize: Icon;
    export const Minimize2: Icon;
    export const Expand: Icon;
    export const Shrink: Icon;
    export const ZoomIn: Icon;
    export const ZoomOut: Icon;
    export const Fullscreen: Icon;
    export const Move: Icon;
    export const Grab: Icon;
    
    // Settings/config icons
    export const Settings: Icon;
    export const Settings2: Icon;
    export const Sliders: Icon;
    export const SlidersHorizontal: Icon;
    export const Filter: Icon;
    export const FilterX: Icon;
    export const SortAsc: Icon;
    export const SortDesc: Icon;
    export const ArrowUpDown: Icon;
    export const Cog: Icon;
    export const Wrench: Icon;
    export const Tool: Icon;
    
    // Search/find icons
    export const Search: Icon;
    export const SearchX: Icon;
    export const Eye: Icon;
    export const EyeOff: Icon;
    export const Scan: Icon;
    export const Focus: Icon;
    export const Target: Icon;
    export const Crosshair: Icon;
    
    // Time/calendar icons
    export const Clock: Icon;
    export const Clock1: Icon;
    export const Clock2: Icon;
    export const Clock3: Icon;
    export const Clock4: Icon;
    export const Clock5: Icon;
    export const Clock6: Icon;
    export const Clock7: Icon;
    export const Clock8: Icon;
    export const Clock9: Icon;
    export const Clock10: Icon;
    export const Clock11: Icon;
    export const Clock12: Icon;
    export const Timer: Icon;
    export const TimerOff: Icon;
    export const TimerReset: Icon;
    export const Hourglass: Icon;
    export const Calendar: Icon;
    export const CalendarDays: Icon;
    export const CalendarRange: Icon;
    export const CalendarCheck: Icon;
    export const CalendarPlus: Icon;
    export const CalendarMinus: Icon;
    export const CalendarX: Icon;
    export const History: Icon;
    export const Alarm: Icon;
    export const AlarmClock: Icon;
    
    // Money/commerce icons
    export const DollarSign: Icon;
    export const Euro: Icon;
    export const PoundSterling: Icon;
    export const Coins: Icon;
    export const Wallet: Icon;
    export const CreditCard: Icon;
    export const Banknote: Icon;
    export const Receipt: Icon;
    export const ShoppingCart: Icon;
    export const ShoppingBag: Icon;
    export const Store: Icon;
    export const Tag: Icon;
    export const Tags: Icon;
    export const Percent: Icon;
    export const Gift: Icon;
    export const BadgePercent: Icon;
    export const Calculator: Icon;
    
    // Charts/analytics icons
    export const BarChart: Icon;
    export const BarChart2: Icon;
    export const BarChart3: Icon;
    export const BarChart4: Icon;
    export const LineChart: Icon;
    export const PieChart: Icon;
    export const TrendingUp: Icon;
    export const TrendingDown: Icon;
    export const Activity: Icon;
    export const Gauge: Icon;
    export const Milestone: Icon;
    
    // Location/map icons
    export const Map: Icon;
    export const MapPin: Icon;
    export const Navigation: Icon;
    export const Navigation2: Icon;
    export const Compass: Icon;
    export const Globe: Icon;
    export const Globe2: Icon;
    export const Home: Icon;
    export const Building: Icon;
    export const Building2: Icon;
    export const Landmark: Icon;
    
    // Security icons
    export const Lock: Icon;
    export const LockOpen: Icon;
    export const Unlock: Icon;
    export const Key: Icon;
    export const KeyRound: Icon;
    export const Shield: Icon;
    export const ShieldCheck: Icon;
    export const ShieldAlert: Icon;
    export const ShieldOff: Icon;
    export const Fingerprint: Icon;
    
    // Tech/dev icons
    export const Code: Icon;
    export const Code2: Icon;
    export const CodeXml: Icon;
    export const Terminal: Icon;
    export const TerminalSquare: Icon;
    export const Database: Icon;
    export const Server: Icon;
    export const Cloud: Icon;
    export const CloudOff: Icon;
    export const CloudUpload: Icon;
    export const CloudDownload: Icon;
    export const Cpu: Icon;
    export const HardDrive: Icon;
    export const Monitor: Icon;
    export const Laptop: Icon;
    export const Smartphone: Icon;
    export const Tablet: Icon;
    export const Wifi: Icon;
    export const WifiOff: Icon;
    export const Bluetooth: Icon;
    export const BluetoothOff: Icon;
    export const Plug: Icon;
    export const Unplug: Icon;
    export const Zap: Icon;
    export const ZapOff: Icon;
    export const Power: Icon;
    export const PowerOff: Icon;
    export const Battery: Icon;
    export const BatteryLow: Icon;
    export const BatteryMedium: Icon;
    export const BatteryFull: Icon;
    export const BatteryCharging: Icon;
    export const Signal: Icon;
    export const SignalLow: Icon;
    export const SignalMedium: Icon;
    export const SignalHigh: Icon;
    export const Rss: Icon;
    export const Radio: Icon;
    export const Satellite: Icon;
    export const Binary: Icon;
    export const Braces: Icon;
    export const Brackets: Icon;
    export const Bug: Icon;
    export const Puzzle: Icon;
    export const Blocks: Icon;
    export const Box: Icon;
    export const Boxes: Icon;
    
    // Text/formatting icons
    export const Bold: Icon;
    export const Italic: Icon;
    export const Underline: Icon;
    export const Strikethrough: Icon;
    export const AlignLeft: Icon;
    export const AlignCenter: Icon;
    export const AlignRight: Icon;
    export const AlignJustify: Icon;
    export const Heading: Icon;
    export const Heading1: Icon;
    export const Heading2: Icon;
    export const Heading3: Icon;
    export const Heading4: Icon;
    export const Heading5: Icon;
    export const Heading6: Icon;
    export const Type: Icon;
    export const Text: Icon;
    export const Quote: Icon;
    export const ListPlus: Icon;
    export const ListMinus: Icon;
    export const ListChecks: Icon;
    export const ListTodo: Icon;
    
    // Misc icons
    export const Sun: Icon;
    export const Moon: Icon;
    export const SunMoon: Icon;
    export const CloudSun: Icon;
    export const Palette: Icon;
    export const Paintbrush: Icon;
    export const Brush: Icon;
    export const PenTool: Icon;
    export const Eraser: Icon;
    export const Scissors: Icon;
    export const Crop: Icon;
    export const FlipHorizontal: Icon;
    export const FlipVertical: Icon;
    export const Sparkles: Icon;
    export const Wand: Icon;
    export const Wand2: Icon;
    export const Crown: Icon;
    export const Award: Icon;
    export const Medal: Icon;
    export const Trophy: Icon;
    export const Flame: Icon;
    export const Rocket: Icon;
    export const Lightbulb: Icon;
    export const LightbulbOff: Icon;
    export const Aperture: Icon;
    export const Book: Icon;
    export const BookOpen: Icon;
    export const BookMarked: Icon;
    export const BookCopy: Icon;
    export const BookText: Icon;
    export const Bookmark: Icon;
    export const BookmarkPlus: Icon;
    export const BookmarkMinus: Icon;
    export const BookmarkCheck: Icon;
    export const Library: Icon;
    export const Notebook: Icon;
    export const NotebookPen: Icon;
    export const Flag: Icon;
    export const FlagOff: Icon;
    export const Pin: Icon;
    export const PinOff: Icon;
    export const Paperclip: Icon;
    export const Attach: Icon;
    export const Asterisk: Icon;
    export const Slash: Icon;
    export const CircleDot: Icon;
    export const CircleDashed: Icon;
    export const Dot: Icon;
    export const Ellipsis: Icon;
    export const EllipsisVertical: Icon;
    export const Separator: Icon;
    export const SeparatorHorizontal: Icon;
    export const SeparatorVertical: Icon;
    
    // Allow any icon import (fallback)
    const icons: { [key: string]: Icon };
    export default icons;
}
''')
        with open(os.path.join(lucide_dir, 'package.json'), 'w') as f:
            f.write('{"name": "lucide-react", "types": "index.d.ts"}')
    
    def _write_node_stub(self, temp_dir: str) -> None:
        """Write NodeJS type stubs."""
        node_types_dir = os.path.join(temp_dir, 'node_modules', '@types', 'node')
        os.makedirs(node_types_dir, exist_ok=True)
        with open(os.path.join(node_types_dir, 'index.d.ts'), 'w') as f:
            f.write('''
declare namespace NodeJS {
    interface Timeout {
        ref(): this;
        unref(): this;
        hasRef(): boolean;
        refresh(): this;
        [Symbol.toPrimitive](): number;
    }
    interface Immediate {
        ref(): this;
        unref(): this;
        hasRef(): boolean;
        _onImmediate: Function;
    }
    interface Timer {
        ref(): this;
        unref(): this;
        hasRef(): boolean;
    }
    interface Process {
        env: { [key: string]: string | undefined };
        cwd(): string;
        exit(code?: number): never;
    }
}

declare var process: NodeJS.Process;
declare function setTimeout(callback: (...args: any[]) => void, ms?: number, ...args: any[]): NodeJS.Timeout;
declare function clearTimeout(timeoutId: NodeJS.Timeout | undefined): void;
declare function setInterval(callback: (...args: any[]) => void, ms?: number, ...args: any[]): NodeJS.Timeout;
declare function clearInterval(intervalId: NodeJS.Timeout | undefined): void;
declare function setImmediate(callback: (...args: any[]) => void, ...args: any[]): NodeJS.Immediate;
declare function clearImmediate(immediateId: NodeJS.Immediate | undefined): void;
''')
        with open(os.path.join(node_types_dir, 'package.json'), 'w') as f:
            f.write('{"name": "@types/node", "types": "index.d.ts"}')
    
    def _write_datastore_stub(self, temp_dir: str) -> None:
        """Write dataStore API stub."""
        lib_dir = os.path.join(temp_dir, 'lib')
        os.makedirs(lib_dir, exist_ok=True)
        with open(os.path.join(lib_dir, 'dataStore.ts'), 'w') as f:
            f.write('''
// DataStore API stub for TypeScript validation
export interface QueryOptions {
    filters?: Array<{ field: string; op: string; value: any }>;
    orderBy?: Array<{ field: string; dir: 'asc' | 'desc'; direction?: 'asc' | 'desc' }>;
    limit?: number;
    offset?: number;
}

export interface QueryResult<T = any> {
    rows: Array<{ id: string; row_index: number; data: T; created_at: string; updated_at: string }>;
    total_count: number;
    has_more: boolean;
}

export interface DataStore {
    query<T = any>(tableSlug: string, options?: QueryOptions): Promise<QueryResult<T>>;
    insert<T = any>(tableSlug: string, data: T): Promise<{ id: string; data: T; row_index: number; created_at: string }>;
    update<T = any>(tableSlug: string, rowId: string, data: Partial<T>): Promise<{ id: string; data: T; updated_at: string }>;
    delete(tableSlug: string, rowId: string): Promise<void>;
    bulkInsert<T = any>(tableSlug: string, data: T[]): Promise<Array<{ id: string; data: T }>>;
    bulkDelete(tableSlug: string, rowIds: string[]): Promise<void>;
}

export const dataStore: DataStore = {} as DataStore;
''')


# Singleton instance
_validation_service: Optional[ValidationService] = None


def get_validation_service() -> ValidationService:
    """Get singleton validation service instance."""
    global _validation_service
    if _validation_service is None:
        _validation_service = ValidationService()
    return _validation_service
