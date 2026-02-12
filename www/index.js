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
    return typeof wasmModule.cddl_from_str === 'function';
  } catch (err) {
    console.error('Failed to load WASM:', err);
    return false;
  }
}

// ─── Validation ────────────────────────────────────────────────────────────────

function validateCDDLText(cddlText) {
  try {
    if (!wasmModule?.cddl_from_str) throw new Error('WASM not ready');
    wasmModule.cddl_from_str(cddlText);
    return { isValid: true, errors: [] };
  } catch (error) {
    const errors = [];

    if (error && typeof error === 'object') {
      if (Array.isArray(error)) {
        error.forEach((e) => errors.push(normaliseError(e)));
      } else if (error.position || error.msg) {
        errors.push(normaliseError(error));
      } else {
        errors.push(normaliseErrorString(error.toString()));
      }
    } else {
      errors.push(normaliseErrorString(String(error)));
    }

    return { isValid: false, errors };
  }
}

function normaliseError(err) {
  const line = err.position?.line || 1;
  const column = err.position?.column || 1;
  const range = err.position?.range || null;
  const raw = err.toString();

  let message = err.msg?.short || err.msg?.extended || raw;

  // Strip diagnostic decoration if present
  if (raw && (raw.includes('┌─') || raw.includes('╰^'))) {
    const extracted = extractDiagnosticMessage(raw);
    if (extracted) message = extracted;
  }

  return classify({ line, column, range, message, raw });
}

function normaliseErrorString(str) {
  let line = 1,
    column = 1;
  let message = str.replace(/^Error:\s*/, '');

  const loc = str.match(/┌─ input:(\d+):(\d+)/);
  if (loc) {
    line = +loc[1];
    column = +loc[2];
    const extracted = extractDiagnosticMessage(str);
    if (extracted) message = extracted;
  } else {
    const lm = str.match(/line (\d+)/i);
    if (lm) line = +lm[1];
    const cm = str.match(/column (\d+)/i);
    if (cm) column = +cm[1];
  }

  return classify({ line, column, range: null, message, raw: str });
}

function extractDiagnosticMessage(diag) {
  const arrow = diag.match(/╰\^\s*(.+)$/m);
  if (arrow) return arrow[1].replace(/[│├└┌┐┘╭╰╯╮─┬┴┼^|]/g, '').trim();

  const lines = diag.split('\n');
  for (let i = lines.length - 1; i >= 0; i--) {
    const t = lines[i]?.trim();
    if (
      t &&
      !/[─│╭╰┌┐└┘^]/.test(t) &&
      !/^\d+\s*│/.test(t) &&
      !/^(Lexer error|Parser error)/i.test(t)
    ) {
      return t.replace(/[│├└┌┐┘╭╰╯╮─┬┴┼^|]/g, '').trim();
    }
  }
  return null;
}

function classify(err) {
  let { message } = err;

  // Clean up
  message = message
    .replace(/^(Lexer error|Parser error|Error):\s*/i, '')
    .trim();
  if (message && message[0] === message[0].toLowerCase()) {
    message = message[0].toUpperCase() + message.slice(1);
  }
  if (message && !/[.!?]$/.test(message)) message += '.';

  let category = 'Syntax Error';
  const src = (err.raw || message).toLowerCase();
  if (/lexer|token|character|symbol/.test(src)) category = 'Lexer Error';
  else if (/parser|syntax|expected|unexpected/.test(src))
    category = 'Parser Error';
  else if (/undefined|not defined|unknown/.test(src))
    category = 'Reference Error';
  else if (/duplicate|already defined/.test(src))
    category = 'Definition Error';

  return { ...err, message, category };
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
  statusText.textContent = 'Checking...';

  const result = validateCDDLText(input);
  updateProblems(result.errors);

  // Update Monaco markers
  const model = editor.getModel();
  const fullText = model.getValue();

  if (result.isValid) {
    monaco.editor.setModelMarkers(model, 'cddl', []);
  } else {
    const markers = result.errors.map((err) => {
      if (err.range && err.range.length === 2) {
        const s = model.getPositionAt(err.range[0]);
        const e = model.getPositionAt(err.range[1]);
        return {
          startLineNumber: s.lineNumber,
          startColumn: s.column,
          endLineNumber: e.lineNumber,
          endColumn: e.column,
          message: err.message,
          severity: monaco.MarkerSeverity.Error,
          code: err.category,
        };
      }
      return {
        startLineNumber: err.line,
        startColumn: err.column,
        endLineNumber: err.line,
        endColumn: Math.min(err.column + 3, 200),
        message: err.message,
        severity: monaco.MarkerSeverity.Error,
        code: err.category,
      };
    });
    monaco.editor.setModelMarkers(model, 'cddl', markers);
  }
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
