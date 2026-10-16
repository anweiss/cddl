export interface WasmDiagnostic {
  position?: { line?: number; column?: number; range?: [number, number]; index?: number };
  msg?: { short?: string; extended?: string };
  severity?: string;
}
export interface Diagnostic {
  line: number;
  column: number;
  range: [number, number] | null;
  message: string;
  category: string;
  severity?: string;
  _openerLine?: number;
  _openerColumn?: number;
  _raw?: WasmDiagnostic;
}
declare global {
  interface Window {
    editor?: import('monaco-editor').editor.IStandaloneCodeEditor;
    jumpToError: (line: number, column: number) => void;
  }
}
