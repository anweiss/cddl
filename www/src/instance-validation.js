// ─── CBOR Input Parsing ──────────────────────────────────────────────────────

/**
 * Parse a user-supplied CBOR string into bytes.
 * Accepts: hex (with or without 0x prefix, whitespace/newlines tolerated,
 * optional colon or space separators) OR base64. Auto-detects which.
 * @returns {{ bytes: Uint8Array|null, error: string|null, encoding: 'hex'|'base64'|null }}
 */
export function parseCborInput(text) {
  const input = String(text ?? '').trim();
  if (!input) {
    return { bytes: null, error: 'CBOR input is empty.', encoding: null };
  }

  const withoutHexPrefixes = input.replace(/(^|[\s:])0x/gi, '$1');
  const hexCandidate = withoutHexPrefixes.replace(/[\s:]/g, '');
  if (hexCandidate && /^[0-9a-fA-F]+$/.test(hexCandidate)) {
    if (hexCandidate.length % 2 !== 0) {
      return { bytes: null, error: 'Hex CBOR input must contain an even number of digits.', encoding: 'hex' };
    }

    const bytes = new Uint8Array(hexCandidate.length / 2);
    for (let i = 0; i < hexCandidate.length; i += 2) {
      bytes[i / 2] = parseInt(hexCandidate.slice(i, i + 2), 16);
    }
    return { bytes, error: null, encoding: 'hex' };
  }

  const base64Candidate = input.replace(/\s/g, '');
  const base64Result = parseBase64(base64Candidate);
  if (base64Result) {
    return { bytes: base64Result, error: null, encoding: 'base64' };
  }

  return {
    bytes: null,
    error: 'CBOR input must be hex bytes or base64-encoded CBOR.',
    encoding: null,
  };
}

function parseBase64(input) {
  if (!input || input.length % 4 === 1 || !/^[A-Za-z0-9+/]*={0,2}$/.test(input)) {
    return null;
  }

  const firstPad = input.indexOf('=');
  if (firstPad !== -1 && !/^=+$/.test(input.slice(firstPad))) {
    return null;
  }

  const padded = input.padEnd(input.length + (4 - input.length % 4) % 4, '=');
  try {
    const binary = typeof globalThis.atob === 'function'
      ? globalThis.atob(padded)
      : globalThis.Buffer.from(padded, 'base64').toString('binary');
    if (!binary) return null;

    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i += 1) {
      bytes[i] = binary.charCodeAt(i);
    }
    return bytes;
  } catch (_) {
    return null;
  }
}

// ─── Instance Validation ─────────────────────────────────────────────────────

/** @returns {boolean} whether CBOR validation is available in this build */
export function isCborSupported(wasmModule) {
  return typeof wasmModule?.validate_cbor_from_slice === 'function';
}

const SYNTHETIC_ROOT_RULE = '__cddl_playground_root';
const RULE_START = /^[ \t]*([A-Za-z@_$][\w@\-.$]*)[ \t]*(<[^>\n]*>)?[ \t]*(\/\/=|\/=|=)[ \t]*(.*)$/;

/**
 * Rewrite `cddl` so the validators use `rootRule` as the root rule.
 * The WASM validators always validate against the first non-generic type rule,
 * so a synthetic alias to the selected rule is prepended when it is not already
 * the root.
 * @returns {{ cddl: string, error: string|null }}
 */
export function applyRootRule(cddl, rootRule) {
  const source = String(cddl ?? '');
  const name = String(rootRule ?? '').trim();
  if (!name) return { cddl: source, error: null };

  const rules = scanRules(source);
  const definitions = rules.filter((rule) => rule.name === name);
  if (definitions.length === 0) return { cddl: source, error: null };

  const firstTypeRule = rules.find((rule) => rule.isRootable);
  if (firstTypeRule && firstTypeRule.name === name) return { cddl: source, error: null };

  if (!definitions.some((rule) => rule.isRootable)) {
    return {
      cddl: source,
      error: `"${name}" is a group or generic rule and cannot be used as a validation root. Select a non-generic type rule.`,
    };
  }

  return { cddl: `${SYNTHETIC_ROOT_RULE} = ${name}\n\n${source}`, error: null };
}

function scanRules(source) {
  const rules = [];
  for (const rawLine of source.split(/\r?\n/)) {
    const line = stripComment(rawLine);
    const match = RULE_START.exec(line);
    if (!match) continue;

    const [, name, generics, assign, rhs] = match;
    rules.push({
      name,
      // Only plain `=` assignments of a non-generic, non-group rule can be a root.
      isRootable: assign === '=' && !generics && !rhs.trimStart().startsWith('('),
    });
  }
  return rules;
}

function stripComment(line) {
  let inString = false;
  for (let i = 0; i < line.length; i += 1) {
    const ch = line[i];
    if (inString) {
      if (ch === '\\') i += 1;
      else if (ch === '"') inString = false;
    } else if (ch === '"') {
      inString = true;
    } else if (ch === ';') {
      return line.slice(0, i);
    }
  }
  return line;
}

/**
 * Validate an instance document against a CDDL schema.
 * @param {object} opts
 * @param {object} opts.wasmModule  the initialised wasm namespace object
 * @param {string} opts.cddl        the schema source
 * @param {'json'|'cbor'} opts.kind
 * @param {string} opts.text        JSON text, or hex/base64 CBOR
 * @param {string} [opts.rootRule]  rule to validate against; defaults to the schema's first type rule
 * @returns {{ ok: boolean, title: string, detail: string, failures: string[], kind: string }}
 */
export function validateInstance(opts) {
  try {
    const wasmModule = opts?.wasmModule;
    const cddl = String(opts?.cddl ?? '');
    const kind = opts?.kind === 'cbor' ? 'cbor' : 'json';
    const text = String(opts?.text ?? '');

    if (!cddl.trim()) {
      return failure('Schema is empty', 'Enter a CDDL schema before validating an instance.', [], kind);
    }

    if (!text.trim()) {
      return failure('Nothing to validate', 'Enter a JSON or CBOR instance to validate.', [], kind);
    }

    const fnName = kind === 'cbor' ? 'validate_cbor_from_slice' : 'validate_json_from_str';
    const validate = wasmModule?.[fnName];
    if (typeof validate !== 'function') {
      const detail = kind === 'cbor'
        ? 'CBOR validation is not available in this WASM build.'
        : 'The WASM module is not loaded or does not expose JSON validation.';
      return failure('WASM module not ready', detail, [detail], kind);
    }

    let instance = text;
    if (kind === 'json') {
      const jsonError = validateJsonText(text);
      if (jsonError) {
        return failure(`Invalid JSON: ${jsonError}`, jsonError, [jsonError], kind);
      }
    } else {
      const parsed = parseCborInput(text);
      if (parsed.error) {
        return failure('Invalid CBOR input', parsed.error, [parsed.error], kind);
      }
      instance = parsed.bytes;
    }

    const rooted = applyRootRule(cddl, opts?.rootRule);
    if (rooted.error) {
      return failure('Unsupported root rule', rooted.error, [rooted.error], kind);
    }

    invokeValidator(validate, rooted.cddl, instance);
    return { ok: true, title: 'Instance is valid', detail: '', failures: [], kind };
  } catch (err) {
    return normaliseValidationFailure(err, opts?.kind === 'cbor' ? 'cbor' : 'json');
  }
}

function invokeValidator(validate, cddl, instance) {
  try {
    return validate(cddl, instance, undefined);
  } catch (err) {
    if (err instanceof TypeError) {
      return validate(cddl, instance);
    }
    throw err;
  }
}

function validateJsonText(text) {
  try {
    JSON.parse(text);
    return null;
  } catch (err) {
    return err?.message || String(err);
  }
}

function normaliseValidationFailure(err, kind) {
  if (Array.isArray(err) && err.length && err.every(isParserErrorLike)) {
    const failures = err.map(formatParserError);
    const detail = err.map(formatParserDetail).join('\n\n');
    return failure('Schema failed to parse', detail, failures, kind);
  }

  const detail = (err instanceof Error ? err.message : String(err)).trimEnd();

  if (kind === 'cbor' && isCborDecodeError(detail)) {
    return failure('Invalid CBOR input', detail, [detail], kind);
  }

  const failures = splitFailures(detail).map(formatConstraintFailure);
  const count = failures.length || 1;
  const plural = count === 1 ? '' : 's';
  return failure(`Instance does not conform (${count} error${plural})`, detail, failures, kind);
}

function isParserErrorLike(err) {
  return Boolean(err && typeof err === 'object' && err.position && err.msg);
}

// Decoding failures surfaced by the WASM CBOR decoder, which are reported
// before any schema constraint is evaluated.
const CBOR_DECODE_ERROR = /^(syntax error at offset\b|unexpected end of input\b|unexpected break\b|i\/o error:)/i;

function isCborDecodeError(detail) {
  return CBOR_DECODE_ERROR.test(String(detail ?? '').trim());
}

function formatParserError(err) {
  const line = err.position?.line ?? 1;
  const column = err.position?.column ?? 1;
  const message = err.msg?.short || err.msg?.extended || 'Unknown parser error';
  return `Ln ${line}, Col ${column}: ${message}`;
}

function formatParserDetail(err) {
  const line = err.position?.line ?? 1;
  const column = err.position?.column ?? 1;
  const message = err.msg?.extended || err.msg?.short || 'Unknown parser error';
  return `Ln ${line}, Col ${column}: ${message}`;
}

function formatConstraintFailure(line) {
  return line
    .replace(/^error validating at JSON location\s+/i, '')
    .replace(/^error validating at the root of the JSON document:\s*/i, 'root of JSON document: ')
    .replace(/^error validating at\s+/i, '');
}

function splitFailures(detail) {
  const failures = String(detail ?? '')
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter(Boolean);
  const fallback = String(detail ?? '').trim() || 'Unknown validation error';
  return failures.length ? failures : [fallback];
}

function failure(title, detail, failures, kind) {
  return {
    ok: false,
    title,
    detail,
    failures: failures.length ? failures : splitFailures(detail),
    kind,
  };
}
