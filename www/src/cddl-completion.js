// ─── CDDL Completion Data ─────────────────────────────────────────────────────
//
// Suggestion sources for the CDDL editor's autocomplete:
//   1. Rule and group names defined in the live buffer.
//   2. The full RFC 8610 Appendix D standard prelude.
//   3. Control operators (RFC 8610 §3.8 plus RFC 9165 additions).
//
// Pure data + string manipulation; no Monaco imports so this stays unit-testable
// and lets `index.js` own the `registerCompletionItemProvider` wiring.

/** RFC 8610 Appendix D — the standard prelude, in spec order. */
export const PRELUDE_TYPES = [
  ['any', 'Any single data item'],
  ['uint', 'Unsigned integer (major type 0)'],
  ['nint', 'Negative integer (major type 1)'],
  ['int', 'Integer — uint / nint'],
  ['bstr', 'Byte string (major type 2)'],
  ['bytes', 'Byte string — alias for bstr'],
  ['tstr', 'Text string (major type 3)'],
  ['text', 'Text string — alias for tstr'],
  ['tdate', 'RFC 3339 date/time string (tag 0)'],
  ['time', 'Epoch-based date/time (tag 1)'],
  ['number', 'int / float'],
  ['biguint', 'Unsigned bignum (tag 2)'],
  ['bignint', 'Negative bignum (tag 3)'],
  ['bigint', 'biguint / bignint'],
  ['integer', 'int / bigint'],
  ['unsigned', 'uint / biguint'],
  ['decfrac', 'Decimal fraction (tag 4)'],
  ['bigfloat', 'Bigfloat (tag 5)'],
  ['eb64url', 'Expected base64url encoding (tag 21)'],
  ['eb64legacy', 'Expected base64 encoding (tag 22)'],
  ['eb16', 'Expected base16 encoding (tag 23)'],
  ['encoded-cbor', 'Encoded CBOR data item (tag 24)'],
  ['uri', 'URI text string (tag 32)'],
  ['b64url', 'base64url-encoded text (tag 33)'],
  ['b64legacy', 'base64-encoded text (tag 34)'],
  ['regexp', 'Regular expression text (tag 35)'],
  ['mime-message', 'MIME message text (tag 36)'],
  ['cbor-any', 'Self-described CBOR (tag 55799)'],
  ['float16', 'IEEE 754 half-precision float'],
  ['float32', 'IEEE 754 single-precision float'],
  ['float64', 'IEEE 754 double-precision float'],
  ['float16-32', 'float16 / float32'],
  ['float32-64', 'float32 / float64'],
  ['float', 'Any IEEE 754 float'],
  ['false', 'Boolean false (simple value 20)'],
  ['true', 'Boolean true (simple value 21)'],
  ['bool', 'false / true'],
  ['nil', 'Null (simple value 22)'],
  ['null', 'Null — alias for nil'],
  ['undefined', 'Undefined (simple value 23)'],
];

/** Control operators — RFC 8610 §3.8, plus `.cborseq` and RFC 9165 `.b64u` family. */
export const CONTROL_OPERATORS = [
  ['.size', 'Constrain the size in bytes (or integer range)', 'tstr .size 12'],
  ['.bits', 'Constrain which bits may be set', 'uint .bits flags'],
  ['.regexp', 'Constrain a text string by XSD regular expression', 'tstr .regexp "[a-z]+"'],
  ['.cbor', 'The byte string carries an embedded CBOR data item', 'bstr .cbor header'],
  ['.cborseq', 'The byte string carries a CBOR sequence', 'bstr .cborseq item'],
  ['.within', 'Constrain to a subset of another type', 'uint .within 0..255'],
  ['.and', 'Value must match both types', 'uint .and my-range'],
  ['.lt', 'Numerically less than', 'uint .lt 10'],
  ['.le', 'Numerically less than or equal to', 'uint .le 10'],
  ['.gt', 'Numerically greater than', 'uint .gt 10'],
  ['.ge', 'Numerically greater than or equal to', 'uint .ge 10'],
  ['.eq', 'Equal to the control value', 'tstr .eq "yes"'],
  ['.ne', 'Not equal to the control value', 'uint .ne 0'],
  ['.default', 'Declare a default value for an optional entry', 'uint .default 0'],
];

const PRELUDE_NAMES = new Set(PRELUDE_TYPES.map(([name]) => name));

// A CDDL rule/group definition at the start of a line:
//   name = ...   name<a, b> = ...   name /= ...   name //= ...
// Rule names may contain letters, digits, and `.`/`-`/`_`/`@`/`$` after the
// first character (RFC 8610 §3.1).
const RULE_DEF = /^[ \t]*([A-Za-z@_$][A-Za-z0-9@_$.\-]*)[ \t]*(<[^>\n]*>)?[ \t]*(\/\/=|\/=|=)(?!=)/gm;

/**
 * Extract rule and group names defined in a CDDL buffer.
 * Returns `[{ name, generic, definition }]` in first-definition order.
 */
export function extractRuleNames(text) {
  if (typeof text !== 'string' || text.length === 0) return [];
  const seen = new Map();
  RULE_DEF.lastIndex = 0;
  let match;
  while ((match = RULE_DEF.exec(text)) !== null) {
    const name = match[1];
    if (seen.has(name)) continue;
    const lineEnd = text.indexOf('\n', match.index);
    const line = text.slice(match.index, lineEnd === -1 ? text.length : lineEnd).trim();
    seen.set(name, {
      name,
      generic: match[2] || '',
      definition: line.length > 120 ? `${line.slice(0, 117)}…` : line,
    });
  }
  return [...seen.values()];
}

/**
 * Is the cursor inside a comment or an unterminated text literal on this line?
 * CDDL comments run from an unquoted `;` to end of line (RFC 8610 §2), so a
 * single left-to-right scan tracking quote state is enough.
 */
export function inCommentOrString(linePrefix) {
  const prefix = typeof linePrefix === 'string' ? linePrefix : '';
  let inString = false;
  for (let i = 0; i < prefix.length; i++) {
    const ch = prefix[i];
    if (inString) {
      if (ch === '\\') i++;
      else if (ch === '"') inString = false;
    } else if (ch === '"') {
      inString = true;
    } else if (ch === ';') {
      return true;
    }
  }
  return inString;
}

/**
 * Decide what to offer given the text on the current line up to the cursor.
 * After a `.` (optionally followed by a partial word) only control operators
 * make sense; everywhere else offer rule names and prelude types. Comments and
 * string literals get nothing.
 *
 * Whitespace is what separates a control operator from a rule name containing a
 * dot: `foo .size` is an operator, `my.rule` is one identifier.
 */
export function completionContext(linePrefix) {
  const prefix = typeof linePrefix === 'string' ? linePrefix : '';
  if (inCommentOrString(prefix)) return 'none';
  if (/\s\.[A-Za-z]*$/.test(prefix) || /^\s*\.[A-Za-z]*$/.test(prefix)) return 'control';
  return 'type';
}

/**
 * Build plain suggestion descriptors. `index.js` maps these onto Monaco's
 * `CompletionItem` shape so this module never imports the editor.
 *
 * @param {string} text        full buffer contents
 * @param {string} linePrefix  text on the current line before the cursor
 * @param {string} [selfRule]  rule currently being defined; excluded from output
 */
export function buildSuggestions(text, linePrefix, selfRule) {
  const kind = completionContext(linePrefix);
  if (kind === 'none') return [];
  if (kind === 'control') {
    return CONTROL_OPERATORS.map(([label, doc, example]) => ({
      label,
      // Monaco has already consumed the leading `.` as part of the word, so
      // insert the operator without it and let the range replacement handle it.
      insertText: label,
      detail: 'control operator',
      documentation: `${doc}\n\nExample: ${example}`,
      group: 'control',
      sortText: `0${label}`,
    }));
  }

  const suggestions = [];
  for (const rule of extractRuleNames(text)) {
    if (rule.name === selfRule) continue;
    if (PRELUDE_NAMES.has(rule.name)) continue;
    suggestions.push({
      label: rule.name,
      insertText: rule.name,
      detail: rule.generic ? `rule ${rule.generic}` : 'rule',
      documentation: rule.definition,
      group: 'rule',
      sortText: `0${rule.name}`,
    });
  }
  for (const [label, doc] of PRELUDE_TYPES) {
    suggestions.push({
      label,
      insertText: label,
      detail: 'prelude',
      documentation: doc,
      group: 'prelude',
      sortText: `1${label}`,
    });
  }
  return suggestions;
}
