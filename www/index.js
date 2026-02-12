import * as monaco from 'monaco-editor';

// ─── CDDL Language Registration ───────────────────────────────────────────────

monaco.languages.register({
  id: 'cddl',
  extensions: ['.cddl'],
  aliases: ['CDDL', 'cddl'],
  mimetypes: ['text/cddl'],
});

monaco.languages.setMonarchTokensProvider('cddl', {
  tokenizer: {
    root: [
      [/;.*$/, 'comment'],
      [
        /\b(any|integer|uint|nint|int|bstr|bytes|tstr|text|tdate|time|number|biguint|bignint|bigint|unsigned|decfrac|bigfloat|eb64url|eb64legacy|eb16|encoded-cbor|uri|b64url|b64legacy|regexp|mime-message|cbor-any|float16|float32|float64|float16-32|float32-64|float|false|true|bool|nil|null|undefined)\b/,
        'keyword',
      ],
      [/\b\d+(\.\d+)?([eE][+-]?\d+)?\b/, 'number'],
      [/#\w+/, 'number'],
      [/"([^"\\]|\\.)*$/, 'string.invalid'],
      [/"/, 'string', '@string'],
      [/'([^'\\]|\\.)*$/, 'string.invalid'],
      [/'/, 'string', '@stringSingle'],
      [/(=|\/\/=|\/=)/, 'operator'],
      [/(=>|:|\^|\?|\+|\*|\/\/|\/)/, 'operator'],
      [
        /\.(size|bits|regexp|pcre|cbor|cborseq|within|and|lt|le|gt|ge|eq|ne|default)/,
        'keyword.operator',
      ],
      [/\.\.\./, 'operator'],
      [/\.\./, 'operator'],
      [/\$\$/, 'keyword.operator'],
      [/\$/, 'keyword.operator'],
      [/[a-zA-Z_][\w-]*/, 'identifier'],
      [/[\[\]]/, 'bracket.square'],
      [/[{}]/, 'bracket.curly'],
      [/[()]/, 'bracket.parenthesis'],
      [/[<>]/, 'bracket.angle'],
      [/,/, 'delimiter'],
      [/[ \t\r\n]+/, 'white'],
    ],
    string: [
      [/[^\\"]+/, 'string'],
      [/\\./, 'string.escape.invalid'],
      [/"/, 'string', '@pop'],
    ],
    stringSingle: [
      [/[^\\']+/, 'string'],
      [/\\./, 'string.escape.invalid'],
      [/'/, 'string', '@pop'],
    ],
  },
});

// Dark theme tuned for the playground
monaco.editor.defineTheme('cddl-dark', {
  base: 'vs-dark',
  inherit: true,
  rules: [
    { token: 'comment', foreground: '6A9955', fontStyle: 'italic' },
    { token: 'keyword', foreground: 'C586C0', fontStyle: 'bold' },
    { token: 'keyword.operator', foreground: 'D16969' },
    { token: 'operator', foreground: 'D4D4D4' },
    { token: 'number', foreground: 'B5CEA8' },
    { token: 'string', foreground: 'CE9178' },
    { token: 'string.invalid', foreground: 'F44747' },
    { token: 'string.escape.invalid', foreground: 'F44747' },
    { token: 'identifier', foreground: '9CDCFE' },
    { token: 'bracket.square', foreground: 'FFD700' },
    { token: 'bracket.curly', foreground: 'DA70D6' },
    { token: 'bracket.parenthesis', foreground: '569CD6' },
    { token: 'bracket.angle', foreground: '4EC9B0' },
    { token: 'delimiter', foreground: '808080' },
  ],
  colors: {
    'editor.background': '#0f1117',
    'editor.foreground': '#e2e8f0',
    'editor.lineHighlightBackground': '#1c1e2d',
    'editor.selectionBackground': '#2a2d5e',
    'editorCursor.foreground': '#6366f1',
    'editorLineNumber.foreground': '#3a3f5c',
    'editorLineNumber.activeForeground': '#6366f1',
    'editor.inactiveSelectionBackground': '#1e2235',
    'editorIndentGuide.background1': '#1e2030',
    'editorIndentGuide.activeBackground1': '#2a2d3e',
  },
});

// Autocomplete for common CDDL types
monaco.languages.registerCompletionItemProvider('cddl', {
  provideCompletionItems: () => {
    const types = [
      ['tstr', 'Text string'],
      ['bstr', 'Byte string'],
      ['uint', 'Unsigned integer'],
      ['int', 'Integer'],
      ['float', 'Floating point number'],
      ['bool', 'Boolean'],
      ['null', 'Null value'],
      ['any', 'Any value'],
      ['text', 'Text string (alias for tstr)'],
      ['bytes', 'Byte string (alias for bstr)'],
    ];
    return {
      suggestions: types.map(([label, doc]) => ({
        label,
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: label,
        documentation: doc,
      })),
    };
  },
});

// ─── WASM Module ──────────────────────────────────────────────────────────────

let wasmModule;

async function initWasm() {
  try {
    const wasmImport = await import('../pkg/cddl.js');
    await wasmImport.default();
    wasmModule = wasmImport;
    return typeof wasmModule.validate_cddl_from_str === 'function';
  } catch (err) {
    console.error('Failed to load WASM:', err);
    return false;
  }
}

// ─── Validation ───────────────────────────────────────────────────────────────
//
// All parsing and partial compilation is handled by the Rust WASM library.
// `validate_cddl_from_str` returns an array of structured errors (empty = valid).
// Each error: { position: { line, column, range: [start, end], index },
//               msg: { short, extended } }
//
// We normalise each into: { line, column, range, message, category }

/**
 * Normalise a WASM error object into the shape the UI expects.
 */
function normaliseError(err) {
  const line = err.position?.line ?? 1;
  const column = err.position?.column ?? 1;
  const range =
    Array.isArray(err.position?.range) && err.position.range.length === 2
      ? err.position.range
      : null;

  let message = err.msg?.short || err.msg?.extended || 'Unknown error';

  // Strip redundant prefixes
  message = message.replace(/^(Lexer error|Parser error|Error)[.:]\s*/i, '').trim();

  // Capitalise first letter
  if (message && /^[a-z]/.test(message)) {
    message = message[0].toUpperCase() + message.slice(1);
  }

  // Ensure sentence-ending punctuation
  if (message && !/[.!?]$/.test(message)) {
    message += '.';
  }

  // Categorise
  const lc = message.toLowerCase();
  let category = 'Syntax';
  if (/\btoken\b|character|symbol|escape|unterminated|byte string|text string|hexfloat|exponent/.test(lc))
    category = 'Lexer';
  else if (/expected|unexpected|missing|invalid.*syntax|must be/.test(lc))
    category = 'Parser';
  else if (/not defined|undefined|unknown/.test(lc))
    category = 'Reference';
  else if (/duplicate|already defined/.test(lc))
    category = 'Duplicate';

  return { line, column, range, message, category };
}

/**
 * Validate CDDL text using the WASM library.
 * Returns { isValid, errors }.
 */
function validateCDDLText(cddlText) {
  if (!wasmModule?.validate_cddl_from_str) {
    return { isValid: false, errors: [{ line: 1, column: 1, range: null, message: 'WASM module not ready.', category: 'Internal' }] };
  }

  try {
    const rawErrors = wasmModule.validate_cddl_from_str(cddlText);
    if (!Array.isArray(rawErrors) || rawErrors.length === 0) {
      return { isValid: true, errors: [] };
    }
    const errors = rawErrors.map(normaliseError);
    return { isValid: false, errors };
  } catch (err) {
    // Unexpected failure — surface it as a single error
    return {
      isValid: false,
      errors: [{ line: 1, column: 1, range: null, message: String(err), category: 'Internal' }],
    };
  }
}

// ─── UI State ──────────────────────────────────────────────────────────────────

const CACHE_KEY = 'cddl-playground-content';

const DEFAULT_EXAMPLE = `; Example CDDL schema
person = {
  name: text,
  age: 0..120,
  ? address: text,
  ? email: text,
}

people = [* person]`;

function loadContent() {
  try {
    const c = localStorage.getItem(CACHE_KEY);
    if (c) return c;
  } catch (_) {}
  return DEFAULT_EXAMPLE;
}

function saveContent(content) {
  try {
    localStorage.setItem(CACHE_KEY, content);
  } catch (_) {}
}

// ─── DOM References ────────────────────────────────────────────────────────────

let editor;
let statusPill, statusText;
let problemsPanel, problemsBadge, problemsBody, problemsEmpty;
let cursorPosition;

// ─── Problems Panel ────────────────────────────────────────────────────────────

function updateProblems(errors) {
  const count = errors.length;

  // Badge
  problemsBadge.textContent = count;
  problemsBadge.classList.toggle('zero', count === 0);

  // Status pill
  statusPill.className = 'status-pill';
  if (count === 0) {
    statusPill.classList.add('valid');
    statusText.textContent = 'Valid';
  } else {
    statusPill.classList.add('invalid');
    statusText.textContent = `${count} error${count > 1 ? 's' : ''}`;
  }

  // Body
  problemsBody.innerHTML = '';

  if (count === 0) {
    problemsBody.appendChild(createEmptyState());
    return;
  }

  // Auto-expand when errors appear
  problemsPanel.classList.remove('collapsed');

  errors.forEach((err) => {
    const row = document.createElement('div');
    row.className = 'problem-row';
    row.onclick = () => jumpTo(err.line, err.column);

    row.innerHTML = `
      <svg class="problem-icon" viewBox="0 0 16 16" fill="currentColor">
        <path d="M8 1a7 7 0 1 0 0 14A7 7 0 0 0 8 1ZM7.25 4.5a.75.75 0 0 1 1.5 0v3.25a.75.75 0 0 1-1.5 0V4.5ZM8 10.5A.75.75 0 1 1 8 12a.75.75 0 0 1 0-1.5Z"/>
      </svg>
      <span class="problem-message">${escapeHtml(err.message)}</span>
      <span class="problem-category">${escapeHtml(err.category)}</span>
      <span class="problem-location">[Ln ${err.line}, Col ${err.column}]</span>
    `;

    problemsBody.appendChild(row);
  });
}

function createEmptyState() {
  const el = document.createElement('div');
  el.className = 'problems-empty';
  el.innerHTML = `
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
      <path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"/>
      <polyline points="22 4 12 14.01 9 11.01"/>
    </svg>
    No problems detected
  `;
  return el;
}

function escapeHtml(str) {
  const el = document.createElement('span');
  el.textContent = str;
  return el.innerHTML;
}

function jumpTo(line, column) {
  if (!editor) return;
  editor.setPosition({ lineNumber: line, column });
  editor.focus();
  editor.revealLineInCenter(line);
}

// ─── Validation loop ───────────────────────────────────────────────────────────

let validationTimer;

function scheduleValidation() {
  clearTimeout(validationTimer);
  validationTimer = setTimeout(runValidation, 250);
}

function runValidation() {
  const input = editor.getValue();
  saveContent(input);

  if (!input.trim()) {
    updateProblems([]);
    monaco.editor.setModelMarkers(editor.getModel(), 'cddl', []);
    statusPill.className = 'status-pill';
    statusText.textContent = 'Ready';
    return;
  }

  // Show checking state briefly
  statusPill.className = 'status-pill checking';
  statusText.textContent = 'Checking\u2026';

  const result = validateCDDLText(input);
  updateProblems(result.errors);

  // Update Monaco markers
  const model = editor.getModel();

  if (result.isValid) {
    monaco.editor.setModelMarkers(model, 'cddl', []);
  } else {
    const markers = result.errors.map((err) => toMarker(model, err));
    monaco.editor.setModelMarkers(model, 'cddl', markers);
  }
}

/**
 * Convert a normalised error into a Monaco IMarkerData.
 *
 * Strategy:
 *  1. If the error has a byte `range`, use it for precise highlighting.
 *  2. Otherwise, use the line/column and try to highlight the token at that
 *     position (scan forward to end of word / operator).
 *  3. Final fallback: highlight from column to end-of-line.
 */
function toMarker(model, err) {
  // ── Range-based (best) ──
  if (err.range && err.range.length === 2 && err.range[1] > err.range[0]) {
    const s = model.getPositionAt(err.range[0]);
    const e = model.getPositionAt(err.range[1]);
    return {
      startLineNumber: s.lineNumber,
      startColumn: s.column,
      endLineNumber: e.lineNumber,
      endColumn: e.column,
      message: err.message,
      severity: monaco.MarkerSeverity.Error,
      source: 'cddl',
    };
  }

  // ── Token-based (good) ──
  const lineNumber = Math.max(1, Math.min(err.line, model.getLineCount()));
  const lineContent = model.getLineContent(lineNumber);
  const col = Math.max(1, Math.min(err.column, lineContent.length + 1));

  // Try to highlight from col to the end of the current "token"
  let endCol = col;
  const rest = lineContent.substring(col - 1);

  if (rest.length > 0) {
    // Match identifier, number, string-quote, or operator cluster
    const tokenMatch = rest.match(/^(?:[a-zA-Z_][\w\-]*|\d[\d.eE+\-]*|"[^"]*"?|'[^']*'?|[^\s,;{}()\[\]]+)/);
    if (tokenMatch) {
      endCol = col + tokenMatch[0].length;
    } else {
      // Single character (bracket, delimiter, etc.)
      endCol = col + 1;
    }
  }

  // Ensure we highlight at least something
  if (endCol <= col) {
    endCol = Math.min(col + 1, lineContent.length + 1);
  }

  // If the error points past the end of the line, highlight the whole line
  if (col > lineContent.length) {
    return {
      startLineNumber: lineNumber,
      startColumn: 1,
      endLineNumber: lineNumber,
      endColumn: lineContent.length + 1,
      message: err.message,
      severity: monaco.MarkerSeverity.Error,
      source: 'cddl',
    };
  }

  return {
    startLineNumber: lineNumber,
    startColumn: col,
    endLineNumber: lineNumber,
    endColumn: endCol,
    message: err.message,
    severity: monaco.MarkerSeverity.Error,
    source: 'cddl',
  };
}

// ─── Initialisation ────────────────────────────────────────────────────────────

function boot() {
  // Grab DOM refs
  statusPill = document.getElementById('statusPill');
  statusText = document.getElementById('statusText');
  problemsPanel = document.getElementById('problemsPanel');
  problemsBadge = document.getElementById('problemsBadge');
  problemsBody = document.getElementById('problemsBody');
  problemsEmpty = document.getElementById('problemsEmpty');
  cursorPosition = document.getElementById('cursorPosition');

  const container = document.getElementById('cddlEditor');
  if (!container) return;

  // Create editor
  editor = monaco.editor.create(container, {
    value: loadContent(),
    language: 'cddl',
    theme: 'cddl-dark',
    fontSize: 14,
    lineHeight: 22,
    fontFamily: "'JetBrains Mono', 'Monaco', 'Menlo', 'Consolas', monospace",
    fontLigatures: true,
    minimap: { enabled: false },
    scrollBeyondLastLine: false,
    automaticLayout: true,
    wordWrap: 'on',
    wrappingIndent: 'indent',
    lineNumbers: 'on',
    glyphMargin: true,
    folding: true,
    renderLineHighlight: 'line',
    cursorBlinking: 'smooth',
    cursorSmoothCaretAnimation: 'on',
    smoothScrolling: true,
    bracketPairColorization: { enabled: true },
    guides: { bracketPairs: 'active', indentation: true },
    padding: { top: 12, bottom: 12 },
    hover: { enabled: true, delay: 300, sticky: true },
  });

  // Expose globally for convenience
  window.editor = editor;

  // Cursor position in status bar
  editor.onDidChangeCursorPosition((e) => {
    cursorPosition.textContent = `Ln ${e.position.lineNumber}, Col ${e.position.column}`;
  });

  // Real-time validation
  editor.onDidChangeModelContent(scheduleValidation);

  // Problems panel toggle
  document.getElementById('problemsHeader').addEventListener('click', () => {
    problemsPanel.classList.toggle('collapsed');
  });

  // Window resize
  window.addEventListener('resize', () => editor.layout());

  // Init WASM then validate
  initWasm().then((ok) => {
    if (ok) {
      runValidation();
    } else {
      statusPill.className = 'status-pill invalid';
      statusText.textContent = 'WASM failed';
    }
  });
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', boot);
} else {
  boot();
}

// Expose jump helper globally (used by inline onclick)
window.jumpToError = jumpTo;
