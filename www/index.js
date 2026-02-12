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
// `validate_cddl_from_str(input, check_refs)` returns an array of structured
// diagnostics (empty = valid).  Each entry has:
//   { position: { line, column, range, index },
//     msg: { short, extended },
//     severity: "error" | "warning" }
//
// We normalise each into: { line, column, range, message, category, severity }

let checkRefs = false;
const REFS_CACHE_KEY = 'cddl-playground-check-refs';

let formatOnSave = false;
const FORMAT_CACHE_KEY = 'cddl-playground-format-on-save';

// ─── Examples Library ─────────────────────────────────────────────────────────

const EXAMPLES = [
  {
    category: 'Basics',
    items: [
      {
        name: 'Hello World',
        desc: 'Minimal schema with a single text rule',
        source: `; The simplest possible CDDL schema
greeting = text
`,
      },
      {
        name: 'Basic Types',
        desc: 'Common CDDL primitive types',
        source: `; Basic CDDL types
name = text
age = uint
temperature = float
active = bool
identifier = bytes
`,
      },
      {
        name: 'Person Record',
        desc: 'Map with required and optional fields',
        source: `; A person record with optional fields
person = {
  name: text,
  age: uint,
  ? email: text,
  ? phone: text,
}
`,
      },
      {
        name: 'Arrays & Ranges',
        desc: 'Array definitions and value constraints',
        source: `; Arrays and value ranges
color = &(red: 1, green: 2, blue: 3)

percentage = 0.0..100.0

rgb = [3 uint .size 1]

palette = [+ color]

scores = [* percentage]
`,
      },
      {
        name: 'Choice Types',
        desc: 'Type alternatives using //',
        source: `; Choice types (type unions)
contact = phone-contact / email-contact / address-contact

phone-contact = {
  type: "phone",
  number: text,
}

email-contact = {
  type: "email",
  address: text,
}

address-contact = {
  type: "address",
  street: text,
  city: text,
  country: text,
}
`,
      },
      {
        name: 'Generics',
        desc: 'Parameterised type definitions',
        source: `; Generic (parameterised) types
paginated<T> = {
  items: [* T],
  page: uint,
  total: uint,
  ? next: uri,
}

user = {
  id: uint,
  name: text,
}

user-page = paginated<user>
`,
      },
    ],
  },
  {
    category: 'Intermediate',
    items: [
      {
        name: 'Socket / Plug',
        desc: 'Extensible types via $/ $$ sockets',
        source: `; Extensible records using socket/plug (RFC 8610 §3.9)
PersonalData = {
  ? displayName: tstr,
  NameComponents,
  ? age: uint,
  * $$personaldata-extensions,
}

NameComponents = (
  ? firstName: tstr,
  ? familyName: tstr,
)

; Extensions can be added from different sources
$$personaldata-extensions //= (
  favorite-salsa: tstr,
)

$$personaldata-extensions //= (
  shoesize: uint,
)
`,
      },
      {
        name: 'Constrained Strings',
        desc: 'Size and regex control operators',
        source: `; String constraints with control operators
hostname = text .regexp "[a-zA-Z][a-zA-Z0-9\\\\-]*(\\\\.[a-zA-Z][a-zA-Z0-9\\\\-]*)*"

email = text .regexp "[a-zA-Z0-9._%+\\\\-]+@[a-zA-Z0-9.\\\\-]+\\\\.[a-zA-Z]{2,}"

short-text = text .size (1..256)

sha256-hash = bytes .size 32

port = uint .size 2

ip4-addr = bytes .size 4

ip6-addr = bytes .size 16
`,
      },
      {
        name: 'Tagged CBOR',
        desc: 'CBOR tags and embedded data',
        source: `; CBOR tags and embedded CBOR
date-time = #6.0(tstr)

epoch-time = #6.1(int / float)

bignum = #6.2(bstr)

uri = #6.32(tstr)

embedded-cbor = #6.24(bstr .cbor inner-structure)

inner-structure = {
  version: uint,
  payload: bytes,
}
`,
      },
      {
        name: 'Occurrence Indicators',
        desc: 'Optional, one-or-more, and exact counts',
        source: `; Occurrence indicators
config = {
  ; Required field (exactly once)
  name: text,

  ; Optional field (zero or one)
  ? description: text,

  ; One or more tags
  tags: [+ text],

  ; Zero or more metadata entries
  metadata: {* text => any},

  ; Exactly 3 priority levels
  priorities: [3 uint],

  ; Between 1 and 5 items
  items: [1*5 item],
}

item = {
  id: uint,
  value: any,
}
`,
      },
    ],
  },
  {
    category: 'Real-World',
    items: [
      {
        name: 'JSON Web Token (JWT)',
        desc: 'Claims set for a JWT/CWT token',
        source: `; JWT / CWT Claims Set (RFC 7519 / RFC 8392)
claims-set = {
  ? iss: text,            ; Issuer
  ? sub: text,            ; Subject
  ? aud: text / [+ text], ; Audience
  ? exp: uint,            ; Expiration Time
  ? nbf: uint,            ; Not Before
  ? iat: uint,            ; Issued At
  ? jti: text,            ; JWT ID
  * text => any,          ; Additional claims
}
`,
      },
      {
        name: 'COSE Key',
        desc: 'COSE Key structure (RFC 9052)',
        source: `; COSE Key Structure (RFC 9052 / RFC 9053)
COSE-Key = {
  1 => kty,              ; Key Type
  ? 2 => bytes,          ; Key ID
  ? 3 => int / text,     ; Algorithm
  ? 4 => [+ key-ops],    ; Key Operations
  ? 5 => bytes,          ; Base IV
  * label => any,
}

kty = int / text

key-ops = int / text

label = int / text

; EC2 key parameters (for P-256, P-384, P-521)
COSE-Key-EC2 = COSE-Key .and {
  -1 => int,             ; Curve
  -2 => bytes,           ; x-coordinate
  ? -3 => bytes,         ; y-coordinate
  ? -4 => bytes,         ; Private key
}
`,
      },
      {
        name: 'Reputon',
        desc: 'Reputation Interchange (RFC 7071)',
        source: `; Reputation Interchange format (RFC 7071)
reputation-object = {
  reputation-context,
  reputon-list,
}

reputation-context = ( application: text )

reputon-list = ( reputons: reputon-array )

reputon-array = [* reputon]

reputon = {
  rater-value,
  assertion-value,
  rated-value,
  rating-value,
  ? conf-value,
  ? normal-value,
  ? sample-value,
  ? gen-value,
  ? expire-value,
  * ext-value,
}

rater-value = ( rater: text )
assertion-value = ( assertion: text )
rated-value = ( rated: text )
rating-value = ( rating: float16 )
conf-value = ( confidence: float16 )
normal-value = ( normal-rating: float16 )
sample-value = ( sample-size: uint )
gen-value = ( generated: uint )
expire-value = ( expires: uint )
ext-value = ( text => any )
`,
      },
      {
        name: 'CoSWID Tag',
        desc: 'Concise Software Identification (RFC 9393)',
        source: `; Concise Software Identification — CoSWID (RFC 9393)
concise-swid-tag = {
  tag-id => text / bstr .size 16,
  tag-version => integer,
  ? corpus => bool,
  ? patch => bool,
  ? supplemental => bool,
  software-name => text,
  ? software-version => text,
  ? version-scheme => $version-scheme,
  entity => entity-entry / [2* entity-entry],
  ? link => link-entry / [2* link-entry],
  * $$coswid-extension,
}

$version-scheme /= &(
  multipartnumeric: 1,
  multipartnumeric-suffix: 2,
  alphanumeric: 3,
  decimal: 4,
  semver: 16384,
)
$version-scheme /= uint / text

entity-entry = {
  entity-name => text,
  ? reg-id => uri,
  role => $role / [2* $role],
  ? thumbprint => hash-entry,
  * $$entity-extension,
}

$role /= &(
  tag-creator: 1,
  software-creator: 2,
  aggregator: 3,
  distributor: 4,
  licensor: 5,
)
$role /= uint / text

link-entry = {
  ? artifact => text,
  href => uri,
  rel => $rel,
  ? media-type => text,
  * $$link-extension,
}

$rel /= &(
  ancestor: -1,
  component: -2,
  feature: -3,
  installationmedia: -4,
  packageinstaller: -5,
  parent: -6,
  patches: -7,
  requires: -8,
  see-also: -9,
  supersedes: -10,
  supplemental: -11,
)
$rel /= int / text

hash-entry = [hash-alg-id: int, hash-value: bytes]

tag-id = 0
tag-version = 12
software-name = 1
software-version = 13
version-scheme = 14
entity = 2
link = 4
corpus = 8
patch = 9
supplemental = 11
entity-name = 31
reg-id = 32
role = 33
thumbprint = 34
artifact = 37
href = 38
rel = 40
media-type = 41
uri = text
`,
      },
      {
        name: 'WebAuthn Attestation',
        desc: 'Web Authentication attestation object',
        source: `; WebAuthn Attestation Object (simplified)
attestation-object = {
  fmt: text,
  attStmt: attestation-statement,
  authData: bytes,
}

attestation-statement = packed-stmt / tpm-stmt / none-stmt

packed-stmt = {
  alg: int,
  sig: bytes,
  ? x5c: [+ bytes],
}

tpm-stmt = {
  alg: int,
  sig: bytes,
  ver: "2.0",
  ? x5c: [+ bytes],
  certInfo: bytes,
  pubArea: bytes,
}

none-stmt = {}

authenticator-data = bytes

credential-public-key = {
  1 => int,              ; kty
  3 => int,              ; alg
  -1 => int,             ; crv (for EC2)
  -2 => bytes,           ; x
  -3 => bytes,           ; y
}

rp-entity = {
  id: text,
  name: text,
  ? icon: text,
}

user-entity = {
  id: bytes .size (1..64),
  name: text,
  displayName: text,
  ? icon: text,
}
`,
      },
      {
        name: 'EAT Token',
        desc: 'Entity Attestation Token (EAT)',
        source: `; Entity Attestation Token (EAT) — simplified
eat-token = {
  ; Standard CWT/JWT claims
  ? 1 => text,             ; iss
  ? 2 => text,             ; sub
  ? 4 => uint,             ; exp
  ? 6 => uint,             ; iat

  ; EAT-specific claims
  ? 10 => nonce,            ; nonce
  ? 256 => ueid,            ; UEID
  ? 258 => oemid,           ; OEM ID
  ? 259 => security-level,
  ? 261 => boot-state,
  ? 265 => [+ swclaim],     ; Software claims
  * label => any,
}

nonce = bytes .size (8..64)

ueid = bytes .size (7..33)

oemid = bytes .size 3 / int

security-level = &(
  unrestricted: 1,
  restricted: 2,
  secure-restricted: 3,
  hardware: 4,
)

boot-state = &(
  secure: 1,
  insecure: 2,
  locked: 3,
)

swclaim = {
  ? 1 => text,       ; measurement type
  ? 2 => bytes,      ; measurement value
  ? 4 => text,       ; version
  ? 5 => uint,       ; signer ID
}

label = int / text
`,
      },
    ],
  },
];

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

  return { line, column, range, message, category, severity: err.severity || 'error' };
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
    const rawErrors = wasmModule.validate_cddl_from_str(cddlText, checkRefs);
    if (!Array.isArray(rawErrors) || rawErrors.length === 0) {
      return { isValid: true, errors: [] };
    }
    const errors = rawErrors.map(normaliseError);
    const hasErrors = errors.some((e) => e.severity === 'error');
    return { isValid: !hasErrors, errors };
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
  let errorCount = 0;
  let warningCount = 0;
  for (let i = 0; i < count; i++) {
    if (errors[i].severity === 'error') errorCount++;
    else warningCount++;
  }

  // Badge
  problemsBadge.textContent = count;
  problemsBadge.classList.toggle('zero', count === 0);

  // Status pill
  statusPill.className = 'status-pill';
  if (errorCount > 0) {
    statusPill.classList.add('invalid');
    const parts = [];
    parts.push(`${errorCount} error${errorCount > 1 ? 's' : ''}`);
    if (warningCount > 0) parts.push(`${warningCount} warning${warningCount > 1 ? 's' : ''}`);
    statusText.textContent = parts.join(', ');
  } else if (warningCount > 0) {
    statusPill.classList.add('valid');
    statusText.textContent = `Valid \u2022 ${warningCount} warning${warningCount > 1 ? 's' : ''}`;
  } else {
    statusPill.classList.add('valid');
    statusText.textContent = 'Valid';
  }

  // Body — batch DOM writes with a document fragment
  if (count === 0) {
    problemsBody.innerHTML = '';
    problemsBody.appendChild(createEmptyState());
    return;
  }

  // Auto-expand when errors appear
  problemsPanel.classList.remove('collapsed');

  const frag = document.createDocumentFragment();
  for (let i = 0; i < count; i++) {
    const err = errors[i];
    const isWarning = err.severity === 'warning';
    const row = document.createElement('div');
    row.className = isWarning ? 'problem-row warning' : 'problem-row';
    row.onclick = () => jumpTo(err.line, err.column);

    const icon = isWarning
      ? `<svg class="problem-icon" viewBox="0 0 16 16" fill="currentColor">
          <path d="M8.22 1.754a.25.25 0 0 0-.44 0L1.698 13.132a.25.25 0 0 0 .22.368h12.164a.25.25 0 0 0 .22-.368Zm-1.763-.707c.659-1.234 2.427-1.234 3.086 0l6.082 11.378A1.75 1.75 0 0 1 14.082 15H1.918a1.75 1.75 0 0 1-1.543-2.575ZM9 11a1 1 0 1 1-2 0 1 1 0 0 1 2 0Zm-.25-5.25a.75.75 0 0 0-1.5 0v2.5a.75.75 0 0 0 1.5 0Z"/>
        </svg>`
      : `<svg class="problem-icon" viewBox="0 0 16 16" fill="currentColor">
          <path d="M8 1a7 7 0 1 0 0 14A7 7 0 0 0 8 1ZM7.25 4.5a.75.75 0 0 1 1.5 0v3.25a.75.75 0 0 1-1.5 0V4.5ZM8 10.5A.75.75 0 1 1 8 12a.75.75 0 0 1 0-1.5Z"/>
        </svg>`;

    row.innerHTML = `${icon}<span class="problem-message">${escapeHtml(err.message)}</span><span class="problem-category">${escapeHtml(err.category)}</span><span class="problem-location">[Ln ${err.line}, Col ${err.column}]</span>`;

    frag.appendChild(row);
  }
  problemsBody.innerHTML = '';
  problemsBody.appendChild(frag);
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

const _escapeMap = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };
const _escapeRe = /[&<>"']/g;
function escapeHtml(str) {
  return str.replace(_escapeRe, (ch) => _escapeMap[ch]);
}

function jumpTo(line, column) {
  if (!editor) return;
  editor.setPosition({ lineNumber: line, column });
  editor.focus();
  editor.revealLineInCenter(line);
}

// ─── CDDL Formatter ────────────────────────────────────────────────────────────

/**
 * Format CDDL source text.
 * - Normalise blank lines (collapse multiple into one)
 * - Indent group entries inside { }, [ ], ( ) by 2 spaces
 * - Trim trailing whitespace
 * - Ensure trailing newline
 */
function formatCDDL(source) {
  const lines = source.split('\n');
  const out = [];
  let depth = 0;
  let prevBlank = false;

  for (let i = 0; i < lines.length; i++) {
    let line = lines[i].trimEnd();
    const stripped = line.trim();

    // Collapse multiple blank lines into one
    if (stripped === '') {
      if (!prevBlank && out.length > 0) out.push('');
      prevBlank = true;
      continue;
    }
    prevBlank = false;

    // Closing delimiters reduce indent before this line
    const leadingClose = stripped.match(/^[\]})]+/);
    if (leadingClose) {
      depth = Math.max(0, depth - leadingClose[0].length);
    }

    // Re-indent
    const indent = '  '.repeat(depth);
    out.push(indent + stripped);

    // Count net openers on this line (ignoring those inside strings/comments)
    const noComment = stripped.replace(/;.*$/, '');
    const noStrings = noComment.replace(/"[^"]*"/g, '').replace(/'[^']*'/g, '');
    for (const ch of noStrings) {
      if (ch === '{' || ch === '[' || ch === '(') depth++;
      else if (ch === '}' || ch === ']' || ch === ')') depth = Math.max(0, depth - 1);
    }
  }

  // Trim trailing blank lines, then add one trailing newline
  while (out.length > 0 && out[out.length - 1] === '') out.pop();
  out.push('');

  return out.join('\n');
}

function applyFormat() {
  if (!editor) return;
  const model = editor.getModel();
  const source = model.getValue();
  const formatted = formatCDDL(source);
  if (formatted !== source) {
    // Use pushEditOperations so it's undoable
    const fullRange = model.getFullModelRange();
    editor.executeEdits('cddl-format', [{
      range: fullRange,
      text: formatted,
    }]);
  }
}

// ─── Validation loop ───────────────────────────────────────────────────────────

let validationTimer;

let lastInput = '';
let saveTimer;

function scheduleValidation() {
  clearTimeout(validationTimer);
  validationTimer = setTimeout(runValidation, 400);
}

function scheduleSave() {
  clearTimeout(saveTimer);
  saveTimer = setTimeout(() => saveContent(lastInput), 1000);
}

function runValidation() {
  const input = editor.getValue();
  // Skip if content hasn't changed since last validation
  if (input === lastInput) return;
  lastInput = input;
  scheduleSave();

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
  const markers = result.errors.map((err) => toMarker(model, err));
  monaco.editor.setModelMarkers(model, 'cddl', markers);
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
  const severity = err.severity === 'warning'
    ? monaco.MarkerSeverity.Warning
    : monaco.MarkerSeverity.Error;

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
      severity,
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

  // If the error points past the end of the line, to an empty/whitespace line,
  // or to a closing delimiter (the real problem is the token before it), walk
  // back to the previous meaningful token — possibly on an earlier line.
  const charAtCol = lineContent[col - 1] || '';
  const isClosingDelim = /^[\]\)}]$/.test(charAtCol);
  if (isClosingDelim) {
    // The error is at a closing delimiter — highlight the gap just before it
    // where the missing token is expected (e.g. the space after `*` in `[* ]`).
    const prefix = lineContent.substring(0, col - 1);
    const trimmedPrefix = prefix.trimEnd();
    // Place a 1-char marker right after the last non-space character
    const insertCol = trimmedPrefix.length + 1;
    return {
      startLineNumber: lineNumber,
      startColumn: insertCol,
      endLineNumber: lineNumber,
      endColumn: insertCol + 1,
      message: err.message,
      severity,
      source: 'cddl',
    };
  }
  if (col > lineContent.length || lineContent.trim().length === 0) {
    // Nothing useful on this line — walk back to the previous non-empty line
    let targetLine = lineNumber;
    let targetContent = lineContent;
    while (targetLine > 1) {
      targetLine--;
      targetContent = model.getLineContent(targetLine);
      if (targetContent.trim().length > 0) break;
    }
    const trimmed = targetContent.trimEnd();
    const lastTokenMatch = trimmed.match(/[a-zA-Z_][\w\-]*$|"[^"]*"$|'[^']*'$|\S+$/);
    if (lastTokenMatch) {
      const start = trimmed.length - lastTokenMatch[0].length + 1;
      return {
        startLineNumber: targetLine,
        startColumn: start,
        endLineNumber: targetLine,
        endColumn: trimmed.length + 1,
        message: err.message,
        severity,
        source: 'cddl',
      };
    }
    return {
      startLineNumber: targetLine,
      startColumn: 1,
      endLineNumber: targetLine,
      endColumn: targetContent.length + 1,
      message: err.message,
      severity,
      source: 'cddl',
    };
  }

  return {
    startLineNumber: lineNumber,
    startColumn: col,
    endLineNumber: lineNumber,
    endColumn: endCol,
    message: err.message,
    severity,
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

  // Ref-check toggle
  const refCheckTrack = document.getElementById('refCheckTrack');
  try {
    checkRefs = localStorage.getItem(REFS_CACHE_KEY) === 'true';
  } catch (_) {}
  if (checkRefs) refCheckTrack.classList.add('active');

  document.getElementById('refCheckToggle').addEventListener('click', () => {
    checkRefs = !checkRefs;
    refCheckTrack.classList.toggle('active', checkRefs);
    try { localStorage.setItem(REFS_CACHE_KEY, checkRefs); } catch (_) {}
    if (editor) { lastInput = ''; runValidation(); }
  });

  // Format-on-save toggle
  const formatTrack = document.getElementById('formatTrack');
  try {
    formatOnSave = localStorage.getItem(FORMAT_CACHE_KEY) === 'true';
  } catch (_) {}
  if (formatOnSave) formatTrack.classList.add('active');

  document.getElementById('formatToggle').addEventListener('click', () => {
    formatOnSave = !formatOnSave;
    formatTrack.classList.toggle('active', formatOnSave);
    try { localStorage.setItem(FORMAT_CACHE_KEY, formatOnSave); } catch (_) {}
  });

  // Examples dropdown
  const examplesWrapper = document.getElementById('examplesWrapper');
  const examplesBtn = document.getElementById('examplesBtn');
  const examplesDropdown = document.getElementById('examplesDropdown');

  const ddFrag = document.createDocumentFragment();
  for (const cat of EXAMPLES) {
    const catEl = document.createElement('div');
    catEl.className = 'examples-category';
    catEl.textContent = cat.category;
    ddFrag.appendChild(catEl);
    for (const ex of cat.items) {
      const item = document.createElement('div');
      item.className = 'example-item';
      item.innerHTML = `<span class="example-item-name">${escapeHtml(ex.name)}</span><span class="example-item-desc">${escapeHtml(ex.desc)}</span>`;
      item.addEventListener('click', () => {
        if (editor) {
          editor.setValue(ex.source);
          lastInput = '';
          runValidation();
        }
        examplesWrapper.classList.remove('open');
      });
      ddFrag.appendChild(item);
    }
  }
  examplesDropdown.appendChild(ddFrag);

  examplesBtn.addEventListener('click', (e) => {
    e.stopPropagation();
    examplesWrapper.classList.toggle('open');
  });

  document.addEventListener('click', (e) => {
    if (!examplesWrapper.contains(e.target)) {
      examplesWrapper.classList.remove('open');
    }
  });

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
    automaticLayout: false,
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

  // Intercept Ctrl/Cmd+S — prevent browser save, format if enabled
  editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, () => {
    if (formatOnSave) applyFormat();
  });

  // Real-time validation
  editor.onDidChangeModelContent(scheduleValidation);

  // Problems panel toggle
  document.getElementById('problemsHeader').addEventListener('click', () => {
    problemsPanel.classList.toggle('collapsed');
    if (!problemsPanel.classList.contains('collapsed')) {
      editor.layout();
    }
  });

  // Problems panel resize
  const resizeHandle = document.getElementById('resizeHandle');
  let resizing = false;

  resizeHandle.addEventListener('mousedown', (e) => {
    if (problemsPanel.classList.contains('collapsed')) return;
    e.preventDefault();
    resizing = true;
    resizeHandle.classList.add('active');
    document.body.style.cursor = 'ns-resize';
    document.body.style.userSelect = 'none';

    const startY = e.clientY;
    const startHeight = problemsPanel.offsetHeight;

    let rafId;
    function onMouseMove(ev) {
      cancelAnimationFrame(rafId);
      rafId = requestAnimationFrame(() => {
        const delta = startY - ev.clientY;
        const newHeight = Math.max(33, Math.min(startHeight + delta, window.innerHeight * 0.7));
        problemsPanel.style.height = newHeight + 'px';
        editor.layout();
      });
    }

    function onMouseUp() {
      resizing = false;
      resizeHandle.classList.remove('active');
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    }

    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);
  });

  // Window resize (throttled)
  let resizeRaf;
  window.addEventListener('resize', () => {
    cancelAnimationFrame(resizeRaf);
    resizeRaf = requestAnimationFrame(() => editor.layout());
  });

  // Initial layout
  editor.layout();

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
