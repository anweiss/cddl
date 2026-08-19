// ─── CDDL Completion Data ─────────────────────────────────────────────────────
//
// Suggestion sources for the CDDL editor's autocomplete:
//   1. Rule and group names defined in the live buffer.
//   2. The full RFC 8610 Appendix D standard prelude.
//   3. Control operators (RFC 8610 §3.8 plus the RFC 9165/9741 and freezer
//      additions enabled by this playground's default WASM build).
//
// Pure data + string manipulation; no Monaco imports so this stays unit-testable
// and lets `index.js` own the `registerCompletionItemProvider` wiring.

import { parseRules } from './outline.js';

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

/**
 * Control operators accepted by the playground's WASM build: RFC 8610 §3.8, the
 * proposed `.pcre` extension, the `freezer` controls, and the RFC 9165/9741
 * additions (`additional-controls` is a default crate feature).
 */
export const CONTROL_OPERATORS = [
  ['.size', 'Constrain the size in bytes (or integer range)', 'tstr .size 12'],
  ['.bits', 'Constrain which bits may be set', 'uint .bits flags'],
  ['.regexp', 'Constrain a text string by XSD regular expression', 'tstr .regexp "[a-z]+"'],
  ['.pcre', 'Constrain a text string by PCRE regular expression', 'tstr .pcre "^[a-z]+$"'],
  ['.iregexp', 'Constrain a text string by an I-Regexp (RFC 9485)', 'tstr .iregexp "[0-9]{4}"'],
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
  ['.bitfield', 'Validate the bitfield layout of a uint (freezer)', 'uint .bitfield flags'],
  ['.cat', 'Concatenate the two literals (RFC 9165)', '"a" .cat "b"'],
  ['.det', 'Concatenate after removing common indentation (RFC 9165)', 'tstr .det doc'],
  ['.plus', 'Numeric addition of the control value (RFC 9165)', 'uint .plus base'],
  ['.abnf', 'Text matches the ABNF grammar (RFC 9165)', 'tstr .abnf rulelist'],
  ['.abnfb', 'Byte string matches the ABNF grammar (RFC 9165)', 'bstr .abnfb rulelist'],
  ['.feature', 'Mark the type as belonging to a named feature (RFC 9165)', 'tstr .feature "v2"'],
  ['.b64u', 'base64url (unpadded) encoding of the control type (RFC 9741)', 'tstr .b64u bstr'],
  ['.b64c', 'base64 classic (padded) encoding of the control type (RFC 9741)', 'tstr .b64c bstr'],
  ['.b64u-sloppy', 'base64url encoding, padding tolerated (RFC 9741)', 'tstr .b64u-sloppy bstr'],
  ['.b64c-sloppy', 'base64 classic encoding, padding tolerated (RFC 9741)', 'tstr .b64c-sloppy bstr'],
  ['.hex', 'base16 encoding in either case (RFC 9741)', 'tstr .hex bstr'],
  ['.hexlc', 'base16 encoding, lowercase (RFC 9741)', 'tstr .hexlc bstr'],
  ['.hexuc', 'base16 encoding, uppercase (RFC 9741)', 'tstr .hexuc bstr'],
  ['.b32', 'base32 encoding of the control type (RFC 9741)', 'tstr .b32 bstr'],
  ['.h32', 'base32hex encoding of the control type (RFC 9741)', 'tstr .h32 bstr'],
  ['.b45', 'base45 encoding of the control type (RFC 9741)', 'tstr .b45 bstr'],
  ['.base10', 'Decimal text representation of an integer (RFC 9741)', 'tstr .base10 int'],
  ['.printf', 'Text formatted per a printf-style specification (RFC 9741)', 'tstr .printf ["%d", int]'],
  ['.json', 'Text is the JSON encoding of the control type (RFC 9741)', 'tstr .json my-type'],
  ['.join', 'Text is the concatenation of an array of literals (RFC 9741)', 'tstr .join parts'],
];

const PRELUDE_NAMES = new Set(PRELUDE_TYPES.map(([name]) => name));

/**
 * Extract rule and group names defined in a CDDL buffer.
 * Delegates to the quote- and comment-aware scanner in `outline.js` so literal
 * content inside comments or (multiline) byte strings is never mistaken for a
 * rule definition.
 * Returns `[{ name, generic, definition }]` in first-definition order.
 */
export function extractRuleNames(text) {
  if (typeof text !== 'string' || text.length === 0) return [];
  const lines = text.split(/\r?\n/);
  const seen = new Map();
  for (const rule of parseRules(text)) {
    if (seen.has(rule.name)) continue;
    const line = (lines[rule.line - 1] || '').trim();
    seen.set(rule.name, {
      name: rule.name,
      generic: rule.generic || '',
      definition: line.length > 120 ? `${line.slice(0, 117)}…` : line,
    });
  }
  return [...seen.values()];
}

/**
 * Is the cursor inside a comment or an unterminated text/byte literal on this
 * line? CDDL comments run from an unquoted `;` to end of line (RFC 8610 §2),
 * and literals are delimited by `"` or `'` (the latter also covering the `h'…'`
 * and `b64'…'` byte string prefixes), so a single left-to-right scan tracking
 * the active delimiter is enough.
 */
export function inCommentOrString(linePrefix) {
  const prefix = typeof linePrefix === 'string' ? linePrefix : '';
  let quote = null;
  for (let i = 0; i < prefix.length; i++) {
    const ch = prefix[i];
    if (quote) {
      if (ch === '\\' && quote === '"') i++;
      else if (ch === quote) quote = null;
    } else if (ch === '"' || ch === "'") {
      quote = ch;
    } else if (ch === ';') {
      return true;
    }
  }
  return quote !== null;
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
  if (/\s\.[A-Za-z0-9-]*$/.test(prefix) || /^\s*\.[A-Za-z0-9-]*$/.test(prefix)) return 'control';
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
