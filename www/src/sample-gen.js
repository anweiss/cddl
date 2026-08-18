/**
 * @typedef {{ json: string|null, warnings: string[], error: string|null, jsonRepresentable: boolean, constraintsSatisfied: boolean, verified: boolean, validationError: string|null }} SampleResult
 */

const MAX_DEPTH = 32;
const MAX_NODES = 5000;
/** Upper bound on repeated occurrences; schemas demanding more fail explicitly. */
const MAX_REPEAT = 64;
/** Structural limits that keep hostile shared links from freezing the tab. */
const MAX_NESTING = 64;
const MAX_TOKENS = 20000;
const BUILTINS = new Set([
  'any', 'uint', 'nint', 'int', 'integer', 'unsigned', 'number', 'float', 'float16',
  'float32', 'float64', 'float16-32', 'float32-64', 'bool', 'true', 'false', 'nil',
  'null', 'tstr', 'text', 'uri', 'tdate', 'time', 'bstr', 'bytes', 'biguint',
  'bignint', 'bigint', 'decfrac', 'bigfloat', 'eb64url', 'eb64legacy', 'eb16',
  'encoded-cbor', 'b64url', 'b64legacy', 'regexp', 'mime-message', 'cbor-any',
  'undefined',
]);

/** Rule names that are plausible entry points, best first. */
export function listRootCandidates(source) {
  try {
    const model = parseSource(source || '');
    if (model.order.length === 0) return [];

    // RFC 8610: the first rule in a schema is conventionally the entry point, so it
    // outranks the "nothing references it" signal, which misfires on real-world schemas.
    const candidates = model.order.filter((name) => !name.startsWith('$'));
    const firstConcrete = candidates.find((name) => model.rules.get(name)?.params.length === 0);
    const referenced = collectReferences(model);
    const roots = candidates.filter((name) => !referenced.has(name));
    const ranked = firstConcrete ? [firstConcrete] : [];

    for (const name of roots) {
      if (!ranked.includes(name)) ranked.push(name);
    }
    for (const name of candidates) {
      if (!ranked.includes(name)) ranked.push(name);
    }

    return ranked;
  } catch {
    return [];
  }
}

/**
 * Generate a sample JSON instance for `rootRuleName` (or the best candidate when omitted).
 *
 * When `options.validate` is supplied it is treated as the source of truth: candidate
 * samples are run through it and the first conforming one wins. Without it the generator
 * can only report its own heuristics, which is why `verified` exists.
 *
 * @param {{ validate?: (json: string) => string|null }} [options]
 *   `validate` returns null when the instance conforms, or the validator error otherwise.
 * @returns {SampleResult} json is pretty-printed with 2-space indent, or null on error.
 */
export function generateSample(source, rootRuleName, options = {}) {
  const validate = typeof options.validate === 'function' ? options.validate : null;
  let last = null;

  // Optional members are a common cause of non-conformance (recursive rules, unsatisfiable
  // controls). Try with them, then without, and let the validator pick the winner.
  for (const includeOptional of [true, false]) {
    const attempt = generateOnce(source, rootRuleName, includeOptional);
    if (attempt.error || !attempt.jsonRepresentable || !validate) return attempt;

    const validationError = validate(attempt.json);
    if (!validationError) return { ...attempt, verified: true, validationError: null };
    last = { ...attempt, verified: false, validationError };
  }

  if (last && last.validationError) {
    last.warnings = [...last.warnings, 'Generated sample does not validate against this schema; it is a best-effort starting point.'];
    last.constraintsSatisfied = false;
  }
  return last;
}

function generateOnce(source, rootRuleName, includeOptional) {
  const warnings = [];

  try {
    const model = parseSource(source || '');
    if (model.order.length === 0) {
      return { json: null, warnings: [], error: 'No rules found in schema', jsonRepresentable: false, constraintsSatisfied: false, verified: false, validationError: null };
    }

    const roots = listRootCandidates(source);
    const root = rootRuleName || roots[0] || model.order[0];
    if (!root || !model.rules.has(root)) {
      warnings.push(`Unknown root rule "${root}"; emitted null.`);
      return { json: JSON.stringify(null, null, 2), warnings, error: null, jsonRepresentable: true, constraintsSatisfied: true, verified: false, validationError: null };
    }

    const ctx = {
      model,
      warnings,
      budget: MAX_NODES,
      jsonRepresentable: true,
      nonRepReasons: new Set(),
      constraintsSatisfied: true,
      unappliedControls: new Set(),
      includeOptional,
    };
    const value = generateRef(root, [], null, ctx, 0, new Set(), new Map());
    return {
      json: JSON.stringify(value, null, 2),
      warnings,
      error: null,
      jsonRepresentable: ctx.jsonRepresentable,
      constraintsSatisfied: ctx.constraintsSatisfied,
      verified: false,
      validationError: null,
    };
  } catch (err) {
    return {
      json: null,
      warnings,
      error: err && err.message ? err.message : String(err),
      jsonRepresentable: false,
      constraintsSatisfied: false,
      verified: false,
      validationError: null,
    };
  }
}

/**
 * Reject pathological input before the recursive-descent parser touches it.
 * A shared permalink is attacker-controlled, and parseExpression recurses per
 * nesting level, so deep nesting is a cheap way to freeze the tab.
 */
function assertParseable(tokens) {
  if (tokens.length > MAX_TOKENS) {
    throw new Error(`Schema is too large to analyze (${tokens.length} tokens).`);
  }

  let depth = 0;
  for (const token of tokens) {
    if (['{', '[', '(', '<'].includes(token.value)) {
      depth += 1;
      if (depth > MAX_NESTING) {
        throw new Error(`Schema nesting is too deep to analyze (over ${MAX_NESTING} levels).`);
      }
    } else if (['}', ']', ')', '>'].includes(token.value)) {
      depth -= 1;
    }
  }
}

function parseSource(source) {
  const tokens = tokenize(source);
  assertParseable(tokens);
  const rules = new Map();
  const order = [];
  let pos = 0;

  while (pos < tokens.length) {
    const start = findNextRuleStart(tokens, pos);
    if (start < 0) break;

    const assign = findRuleAssign(tokens, start);
    if (assign < 0) break;

    const name = tokens[start].value;
    const params = parseParams(tokens.slice(start + 1, assign));
    const next = findNextRuleStart(tokens, assign + 1);
    const rhsTokens = tokens.slice(assign + 1, next < 0 ? tokens.length : next);
    const node = parseExpression(rhsTokens);
    const op = tokens[assign].value;

    if (!rules.has(name)) {
      const initialNode = op === '//='
        ? { kind: 'groupMerge', options: [node], label: name }
        : op === '/='
          ? { kind: 'choice', options: [node], label: name }
          : node;
      rules.set(name, { name, params, node: initialNode, choices: op === '=' ? [] : [node] });
      order.push(name);
      pos = next < 0 ? tokens.length : next;
      continue;
    }

    const rule = rules.get(name);
    if (op === '=') {
      rule.params = params;
      rule.node = node;
      rule.choices = [];
    } else if (op === '//=') {
      rule.choices.push(node);
      rule.node = rule.node.kind === 'groupMerge'
        ? { ...rule.node, options: [...rule.node.options, node] }
        : { kind: 'groupMerge', options: [rule.node, node], label: name };
    } else {
      rule.choices.push(node);
      rule.node = rule.node.kind === 'choice'
        ? { ...rule.node, options: [...rule.node.options, node] }
        : { kind: 'choice', options: [rule.node, node], label: name };
    }

    pos = next < 0 ? tokens.length : next;
  }

  return { rules, order };
}

function tokenize(source) {
  const tokens = [];
  let i = 0;

  while (i < source.length) {
    const ch = source[i];

    if (/\s/.test(ch)) { i++; continue; }
    if (ch === ';') {
      while (i < source.length && source[i] !== '\n') i++;
      continue;
    }

    if (ch === '"' || ch === "'") {
      const result = readQuoted(source, i, ch);
      tokens.push(result.token);
      i = result.next;
      continue;
    }

    if ((source.startsWith("h'", i) || source.startsWith("b64'", i)) && /[hb]/.test(ch)) {
      const prefix = source.startsWith("b64'", i) ? 'b64' : 'h';
      const result = readQuoted(source, i + prefix.length, "'");
      tokens.push({ type: 'bytes', value: source.slice(i, result.next), raw: source.slice(i, result.next) });
      i = result.next;
      continue;
    }

    const three = source.slice(i, i + 3);
    const two = source.slice(i, i + 2);
    if (['//=', '...', '=>'].includes(three)) {
      tokens.push({ type: 'op', value: three, raw: three });
      i += 3;
      continue;
    }
    if (['/=', '=>', '..', '//'].includes(two)) {
      tokens.push({ type: 'op', value: two, raw: two });
      i += 2;
      continue;
    }

    if (ch === '.' && /[A-Za-z]/.test(source[i + 1] || '')) {
      let j = i + 1;
      while (j < source.length && /[A-Za-z0-9_-]/.test(source[j])) j++;
      tokens.push({ type: 'control', value: source.slice(i, j), raw: source.slice(i, j) });
      i = j;
      continue;
    }

    if (isNumberStart(source, i)) {
      const match = source.slice(i).match(/^-?(?:0x[0-9a-fA-F]+(?:\.[0-9a-fA-F]+)?p[+-]?\d+|0x[0-9a-fA-F]+|0b[01]+|(?:[1-9]\d*|0)(?:\.\d+)?(?:[eE][+-]?\d+)?)/);
      if (match) {
        tokens.push({ type: 'number', value: match[0], raw: match[0] });
        i += match[0].length;
        continue;
      }
    }

    if (isIdentStart(ch) || ch === '$') {
      let j = i;
      if (source.startsWith('$$', i)) j += 2;
      else if (source[j] === '$') j++;
      while (j < source.length && /[A-Za-z0-9_@$.\-]/.test(source[j])) j++;
      tokens.push({ type: 'id', value: source.slice(i, j), raw: source.slice(i, j) });
      i = j;
      continue;
    }

    if ('={}[](),:<>/?*+~&#^'.includes(ch)) {
      tokens.push({ type: 'op', value: ch, raw: ch });
      i++;
      continue;
    }

    i++;
  }

  return tokens;
}

function readQuoted(source, start, quote) {
  let i = start + 1;
  while (i < source.length) {
    if (source[i] === '\\') { i += 2; continue; }
    if (source[i] === quote) { i++; break; }
    i++;
  }
  const raw = source.slice(start, i);
  return { token: { type: quote === '"' ? 'string' : 'bytes', value: raw, raw }, next: i };
}

function isNumberStart(source, i) {
  return /\d/.test(source[i]) || (source[i] === '-' && /\d/.test(source[i + 1] || ''));
}

function isIdentStart(ch) {
  return /[A-Za-z_@]/.test(ch);
}

function findNextRuleStart(tokens, from) {
  for (let i = from; i < tokens.length; i++) {
    if (tokens[i].type === 'id' && findRuleAssign(tokens, i) >= 0) return i;
  }
  return -1;
}

function findRuleAssign(tokens, start) {
  if (!tokens[start] || tokens[start].type !== 'id') return -1;
  let i = start + 1;
  if (tokens[i]?.value === '<') i = skipBalanced(tokens, i, '<', '>') + 1;
  return ['=', '/=', '//='].includes(tokens[i]?.value) ? i : -1;
}

function skipBalanced(tokens, start, open, close) {
  let depth = 0;
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].value === open) depth++;
    else if (tokens[i].value === close && --depth === 0) return i;
  }
  return start;
}

function parseParams(tokens) {
  const params = [];
  if (tokens[0]?.value !== '<') return params;
  for (const token of tokens) {
    if (token.type === 'id') params.push(token.value);
  }
  return params;
}

function parseExpression(tokens) {
  const trimmed = trimTokens(tokens);
  if (trimmed.length === 0) return { kind: 'unknown', name: 'empty expression' };

  const choiceParts = splitTopLevel(trimmed, '/');
  if (choiceParts.length > 1) {
    return { kind: 'choice', options: choiceParts.map(parseExpression), label: describeTokens(trimmed) };
  }

  const rangeIndex = findTopLevelOp(trimmed, ['..', '...']);
  if (rangeIndex >= 0) {
    return {
      kind: 'range',
      inclusive: trimmed[rangeIndex].value === '..',
      lower: parseExpression(trimmed.slice(0, rangeIndex)),
      upper: parseExpression(trimmed.slice(rangeIndex + 1)),
    };
  }

  const controlIndex = findTopLevelControl(trimmed);
  if (controlIndex >= 0) {
    return {
      kind: 'control',
      op: trimmed[controlIndex].value,
      base: parseExpression(trimmed.slice(0, controlIndex)),
      controller: parseExpression(trimmed.slice(controlIndex + 1)),
    };
  }

  if (isWrapped(trimmed, '(', ')')) {
    const inner = trimmed.slice(1, -1);
    if (findTopLevelOp(inner, [':', '=>']) >= 0) return { kind: 'map', entries: parseGroup(inner) };
    // `( int, tstr )` is a group of entries, not a parenthesized single type.
    if (splitTopLevel(inner, ',').length > 1) return { kind: 'group', entries: parseGroup(inner) };
    return parseExpression(inner);
  }
  if (isWrapped(trimmed, '{', '}')) return { kind: 'map', entries: parseGroup(trimmed.slice(1, -1)) };
  if (isWrapped(trimmed, '[', ']')) return { kind: 'array', entries: parseGroup(trimmed.slice(1, -1)) };

  if (trimmed[0]?.value === '~') {
    const ref = parseReference(trimmed.slice(1));
    return { kind: 'unwrap', ref };
  }

  if (trimmed[0]?.value === '&') {
    if (isWrapped(trimmed.slice(1), '(', ')')) {
      return { kind: 'enum', entries: parseGroup(trimmed.slice(2, -1)) };
    }
    return { kind: 'enumRef', ref: parseReference(trimmed.slice(1)) };
  }

  if (trimmed[0]?.value === '#') {
    const paren = trimmed.findIndex((token) => token.value === '(');
    if (paren >= 0 && trimmed[trimmed.length - 1]?.value === ')') {
      return { kind: 'tag', inner: parseExpression(trimmed.slice(paren + 1, -1)), tag: describeTokens(trimmed.slice(0, paren)) };
    }
    return { kind: 'tag', inner: { kind: 'ref', name: 'any', args: [] }, tag: describeTokens(trimmed) };
  }

  if (trimmed.length === 1) return parseAtom(trimmed[0]);
  return parseReference(trimmed);
}

function trimTokens(tokens) {
  return tokens.filter(Boolean);
}

function splitTopLevel(tokens, op) {
  const parts = [];
  let start = 0;
  let depth = 0;
  for (let i = 0; i < tokens.length; i++) {
    const value = tokens[i].value;
    if (['{', '[', '(', '<'].includes(value)) depth++;
    else if (['}', ']', ')', '>'].includes(value)) depth--;
    else if (depth === 0 && value === op) {
      parts.push(tokens.slice(start, i));
      start = i + 1;
    }
  }
  if (parts.length > 0) parts.push(tokens.slice(start));
  return parts.length > 0 ? parts : [tokens];
}

function findTopLevelOp(tokens, ops) {
  let depth = 0;
  for (let i = 0; i < tokens.length; i++) {
    const value = tokens[i].value;
    if (['{', '[', '(', '<'].includes(value)) depth++;
    else if (['}', ']', ')', '>'].includes(value)) depth--;
    else if (depth === 0 && ops.includes(value)) return i;
  }
  return -1;
}

function findTopLevelControl(tokens) {
  let depth = 0;
  for (let i = 0; i < tokens.length; i++) {
    const value = tokens[i].value;
    if (['{', '[', '(', '<'].includes(value)) depth++;
    else if (['}', ']', ')', '>'].includes(value)) depth--;
    else if (depth === 0 && tokens[i].type === 'control') return i;
  }
  return -1;
}

function isWrapped(tokens, open, close) {
  return tokens[0]?.value === open && matchingCloseIndex(tokens, 0) === tokens.length - 1 && tokens.at(-1)?.value === close;
}

function matchingCloseIndex(tokens, start) {
  const pairs = { '{': '}', '[': ']', '(': ')', '<': '>' };
  const open = tokens[start]?.value;
  const close = pairs[open];
  let depth = 0;
  for (let i = start; i < tokens.length; i++) {
    if (tokens[i].value === open) depth++;
    else if (tokens[i].value === close && --depth === 0) return i;
  }
  return -1;
}

function parseGroup(tokens) {
  const choices = splitTopLevel(tokens, '//');
  if (choices.length > 1) {
    return [{ kind: 'groupChoice', options: choices.map(parseGroup) }];
  }

  return splitTopLevel(tokens, ',')
    .flatMap(splitImplicitGroupEntries)
    .map((entry) => parseGroupEntry(entry))
    .filter(Boolean);
}

function splitImplicitGroupEntries(tokens) {
  tokens = trimTokens(tokens);
  if (tokens.length < 2 || findTopLevelOp(tokens, [':', '=>']) >= 0) return [tokens];
  if (tokens[0].type === 'number' && tokens[1]?.value !== '*' && tokens[1]?.value !== '..' && tokens[1]?.value !== '...') {
    return [tokens.slice(0, 1), ...splitImplicitGroupEntries(tokens.slice(1))];
  }
  return [tokens];
}

function parseGroupEntry(tokens) {
  tokens = trimTokens(tokens);
  if (tokens.length === 0) return null;

  let occurrence = { min: 1, max: 1, count: 1, optional: false, raw: '' };
  const occ = readOccurrence(tokens);
  if (occ) {
    occurrence = occ.occurrence;
    tokens = tokens.slice(occ.consumed);
  }

  if (isWrapped(tokens, '(', ')')) {
    return { kind: 'group', occurrence, entries: parseGroup(tokens.slice(1, -1)) };
  }

  const opIndex = findTopLevelOp(tokens, [':', '=>']);
  if (opIndex > 0) {
    return {
      kind: 'member',
      occurrence,
      key: parseKey(tokens.slice(0, opIndex)),
      value: parseExpression(tokens.slice(opIndex + 1)),
      arrow: tokens[opIndex].value === '=>',
    };
  }

  return { kind: 'element', occurrence, value: parseExpression(tokens) };
}

function readOccurrence(tokens) {
  const first = tokens[0];
  if (!first) return null;
  if (first.value === '?') return { consumed: 1, occurrence: makeOccurrence(0, 1, '?', true) };
  if (first.value === '*' && tokens[1]?.type !== 'number') return { consumed: 1, occurrence: makeOccurrence(0, Infinity, '*') };
  if (first.value === '+') return { consumed: 1, occurrence: makeOccurrence(1, Infinity, '+') };

  if (first.type === 'number' && tokens[1]?.value === '*' && tokens[2]?.type === 'number') {
    return {
      consumed: 3,
      occurrence: makeOccurrence(numberFromToken(first), numberFromToken(tokens[2]), `${first.raw}*${tokens[2].raw}`),
    };
  }

  if (first.type === 'number' && tokens[1]?.value === '*') {
    return {
      consumed: 2,
      occurrence: makeOccurrence(numberFromToken(first), Infinity, `${first.raw}*`),
    };
  }

  if (first.value === '*' && tokens[1]?.type === 'number') {
    return {
      consumed: 2,
      occurrence: makeOccurrence(0, numberFromToken(tokens[1]), `*${tokens[1].raw}`),
    };
  }

  return null;
}

/** Occurrence with a concrete repeat count that always respects [min, max]. */
function makeOccurrence(min, max, raw, optional = false) {
  const lo = Number.isFinite(min) && min >= 0 ? min : 0;
  const hi = max === Infinity || (Number.isFinite(max) && max >= lo) ? max : lo;
  const count = Math.max(lo, Math.min(1, hi));
  return { min: lo, max: hi, count, optional, raw };
}

/** Repeat count clamped to MAX_REPEAT; exceeding the cap is reported, never silent. */
function repeatCount(occurrence, ctx) {
  const wanted = occurrence?.count ?? 1;
  if (wanted <= MAX_REPEAT) return wanted;
  ctx.constraintsSatisfied = false;
  ctx.warnings.push(`Occurrence "${occurrence.raw}" requires ${wanted} items; generated ${MAX_REPEAT} to keep the sample manageable.`);
  return MAX_REPEAT;
}

/** True when an optional entry should be omitted for this attempt. */
function skipOptional(entry, ctx) {
  return ctx.includeOptional === false && entry?.occurrence?.optional === true;
}

/** Assign without invoking inherited setters such as `__proto__`. */
function setMember(object, key, value) {
  Object.defineProperty(object, key, { value, enumerable: true, writable: true, configurable: true });
}

/** Object.assign equivalent that is safe for `__proto__` keys. */
function mergeMembers(target, source) {
  if (!source || typeof source !== 'object') return target;
  for (const key of Object.keys(source)) setMember(target, key, source[key]);
  return target;
}

function parseKey(tokens) {
  tokens = trimTokens(tokens).filter((token) => token.value !== '^');
  if (tokens.length === 1) {
    const token = tokens[0];
    if (token.type === 'string') return { kind: 'literal', value: parseStringToken(token), raw: token.raw };
    if (token.type === 'number') return { kind: 'literal', value: numberFromToken(token), raw: token.raw };
    if (token.type === 'id') return { kind: 'name', name: token.value };
  }
  return { kind: 'computed', node: parseExpression(tokens), text: describeTokens(tokens) };
}

function parseAtom(token) {
  if (token.type === 'string') return { kind: 'literal', value: parseStringToken(token), raw: token.raw };
  if (token.type === 'bytes') return { kind: 'bytesLiteral', raw: token.raw };
  if (token.type === 'number') return { kind: 'literal', value: numberFromToken(token), raw: token.raw };
  if (token.type === 'id') {
    if (token.value === 'true') return { kind: 'literal', value: true, raw: 'true' };
    if (token.value === 'false') return { kind: 'literal', value: false, raw: 'false' };
    if (token.value === 'null' || token.value === 'nil') return { kind: 'literal', value: null, raw: token.value };
    return { kind: 'ref', name: token.value, args: [] };
  }
  return { kind: 'unknown', name: token.raw };
}

function parseReference(tokens) {
  tokens = trimTokens(tokens);
  const name = tokens[0]?.type === 'id' ? tokens[0].value : describeTokens(tokens);
  const args = [];
  const angle = tokens.findIndex((token) => token.value === '<');
  if (angle >= 0 && tokens.at(-1)?.value === '>') {
    for (const part of splitTopLevel(tokens.slice(angle + 1, -1), ',')) {
      args.push(parseExpression(part));
    }
  }
  return { kind: 'ref', name, args };
}

function parseStringToken(token) {
  try { return JSON.parse(token.raw); } catch { return token.raw.slice(1, -1); }
}

function numberFromToken(token) {
  const raw = token.raw || token.value;
  if (/^-?0x/i.test(raw)) return parseInt(raw, 16);
  if (/^-?0b/i.test(raw)) return parseInt(raw.replace(/^-/i, '').slice(2), 2) * (raw.startsWith('-') ? -1 : 1);
  const number = Number(raw);
  return Number.isFinite(number) ? number : 0;
}

function generateRef(name, args, key, ctx, depth, visiting, env) {
  if (env.has(name)) return generateNode(env.get(name), key, ctx, depth + 1, visiting, env, name);
  if (BUILTINS.has(name)) return generateBuiltin(name, key, ctx);
  if (depth > MAX_DEPTH) {
    ctx.warnings.push(`Recursion depth exceeded while resolving "${name}"; emitted null.`);
    return null;
  }
  if (visiting.has(name)) {
    ctx.warnings.push(`Cycle detected while resolving "${name}"; emitted null.`);
    return null;
  }

  const rule = ctx.model.rules.get(name);
  if (!rule) {
    ctx.warnings.push(`Unknown rule "${name}"; emitted null.`);
    return null;
  }

  const nextEnv = new Map(env);
  rule.params.forEach((param, index) => {
    if (args[index]) nextEnv.set(param, args[index]);
  });

  const nextVisiting = new Set(visiting);
  nextVisiting.add(name);
  return generateNode(rule.node, key, ctx, depth + 1, nextVisiting, nextEnv, name);
}

function generateNode(node, key, ctx, depth, visiting, env, label) {
  if (!node) return null;
  if (--ctx.budget < 0) {
    ctx.warnings.push('Sample generation node budget exhausted; emitted null.');
    return null;
  }
  if (depth > MAX_DEPTH) {
    ctx.warnings.push(`Recursion depth exceeded${label ? ` at "${label}"` : ''}; emitted null.`);
    return null;
  }

  switch (node.kind) {
    case 'literal': return node.value;
    case 'bytesLiteral':
      markNotJsonRepresentable(ctx, 'byte strings');
      ctx.warnings.push(`Byte string literal ${node.raw} represented as a JSON string.`);
      return 'AQID';
    case 'ref': return generateRef(node.name, node.args, key, ctx, depth, visiting, env);
    case 'unwrap': return generateNode(node.ref, key, ctx, depth + 1, visiting, env, label);
    case 'range': return generateNode(node.lower, key, ctx, depth + 1, visiting, env, label);
    case 'choice': {
      const option = chooseOption(node.options, ctx.model, env);
      ctx.warnings.push(`Using ${option === node.options[0] ? 'first' : 'a later'} alternative of choice${node.label ? ` "${node.label}"` : ''}.`);
      if (option !== node.options[0]) ctx.warnings.push('Skipped an earlier choice alternative with a prelude-name map key that does not validate as a JSON object key.');
      return generateNode(option, key, ctx, depth + 1, visiting, env, label);
    }
    case 'control': return generateControl(node, key, ctx, depth, visiting, env, label);
    case 'tag':
      markNotJsonRepresentable(ctx, 'CBOR tags');
      ctx.warnings.push(`CBOR tag ${node.tag || ''} was dropped for JSON output.`.trim());
      return generateNode(node.inner, key, ctx, depth + 1, visiting, env, label);
    case 'map': return generateMap(node.entries, ctx, depth + 1, visiting, env);
    case 'array': return generateArray(node.entries, ctx, depth + 1, visiting, env);
    case 'group':
      return hasMemberEntries(node.entries)
        ? generateMap(node.entries, ctx, depth + 1, visiting, env)
        : generateArray(node.entries, ctx, depth + 1, visiting, env);
    case 'enum': return generateEnum(node.entries, ctx, depth + 1, visiting, env);
    case 'groupMerge': return generateGroupMerge(node.options, ctx, depth + 1, visiting, env);
    case 'enumRef': {
      const value = generateNode(node.ref, key, ctx, depth + 1, visiting, env, label);
      return value && typeof value === 'object' && !Array.isArray(value) ? Object.values(value)[0] ?? null : value;
    }
    case 'unknown':
    default:
      ctx.warnings.push(`Unsupported construct "${node.name || node.kind}"; emitted null.`);
      return null;
  }
}

function generateControl(node, key, ctx, depth, visiting, env, label) {
  const base = () => generateNode(node.base, key, ctx, depth + 1, visiting, env, label);
  const controller = () => generateNode(node.controller, key, ctx, depth + 1, visiting, env, label);

  switch (node.op) {
    case '.default':
    case '.eq':
      return controller();
    case '.within':
      return base();
    case '.size': {
      const size = controlNumber(node.controller);
      const value = base();
      if (size == null || size < 0) return markControlUnapplied(ctx, node.op, value);
      if (typeof value === 'string') return sizedString(value, size);
      if (typeof value === 'number') return Number.isInteger(value) ? sizedInteger(value, size) : value;
      return markControlUnapplied(ctx, node.op, value);
    }
    case '.lt':
    case '.le':
    case '.gt':
    case '.ge': {
      const limit = controlNumber(node.controller);
      const value = base();
      if (limit == null || typeof value !== 'number') return markControlUnapplied(ctx, node.op, value);
      return boundedNumber(value, limit, node.op);
    }
    case '.ne': {
      const value = base();
      const excluded = controller();
      if (value !== excluded) return value;
      if (typeof value === 'number') return value + 1;
      if (typeof value === 'string') return `${value}x`;
      if (typeof value === 'boolean') return !value;
      return markControlUnapplied(ctx, node.op, value);
    }
    default:
      return markControlUnapplied(ctx, node.op, base());
  }
}

function markControlUnapplied(ctx, op, value) {
  ctx.constraintsSatisfied = false;
  if (!ctx.unappliedControls.has(op)) {
    ctx.unappliedControls.add(op);
    ctx.warnings.push(`Control operator ${op} could not be applied; the generated value may not validate.`);
  }
  return value;
}

/** Numeric controller value, or null when it is not a plain number or numeric range. */
function controlNumber(node) {
  if (!node) return null;
  if (node.kind === 'literal' && typeof node.value === 'number') return node.value;
  if (node.kind === 'range') return controlNumber(node.lower);
  return null;
}

function sizedString(value, size) {
  if (value.length === size) return value;
  if (value.length > size) return value.slice(0, size);
  return value.padEnd(size, 'x');
}

/** Largest-fitting placeholder for `n .size b`, where b counts bytes of the encoded integer. */
function sizedInteger(value, size) {
  if (size === 0) return 0;
  const max = 2 ** (Math.min(size, 6) * 8) - 1;
  if (value >= 0) return value <= max ? value : max;
  return value >= -max - 1 ? value : -max - 1;
}

function boundedNumber(value, limit, op) {
  switch (op) {
    case '.lt': return value < limit ? value : limit - 1;
    case '.le': return value <= limit ? value : limit;
    case '.gt': return value > limit ? value : limit + 1;
    default: return value >= limit ? value : limit;
  }
}

function generateBuiltin(name, key, ctx) {
  if (['text', 'tstr', 'uri', 'tdate', 'time', 'regexp', 'mime-message'].includes(name)) {
    return stringForKey(key || name);
  }
  if (['uint', 'unsigned', 'biguint'].includes(name)) return 1;
  if (['nint', 'bignint'].includes(name)) return -1;
  if (['int', 'integer', 'bigint', 'number'].includes(name)) return 0;
  if (['float', 'float16', 'float32', 'float64', 'float16-32', 'float32-64', 'decfrac', 'bigfloat'].includes(name)) return 1.5;
  if (['bool', 'true', 'false'].includes(name)) return true;
  if (['null', 'nil', 'undefined'].includes(name)) return null;
  if (['bytes', 'bstr', 'eb64url', 'eb64legacy', 'eb16', 'encoded-cbor', 'b64url', 'b64legacy'].includes(name)) {
    markNotJsonRepresentable(ctx, 'byte strings');
    ctx.warnings.push(`JSON cannot represent CBOR byte strings natively; ${name} emitted as a base64-like string.`);
    return 'AQID';
  }
  if (['any', 'cbor-any'].includes(name)) {
    ctx.warnings.push(`${name} cannot be inferred; emitted a placeholder string.`);
    return 'any';
  }
  ctx.warnings.push(`Unsupported prelude type "${name}"; emitted null.`);
  return null;
}

function chooseOption(options, model, env) {
  return options.find((option) => !hasPreludeNameKey(option, model, env, new Set())) || options[0];
}

function hasPreludeNameKey(node, model, env, seen) {
  if (!node) return false;
  if (node.kind === 'ref') {
    if (env?.has(node.name)) return hasPreludeNameKey(env.get(node.name), model, env, seen);
    if (seen.has(node.name)) return false;
    seen.add(node.name);
    const rule = model?.rules.get(node.name);
    return rule ? hasPreludeNameKey(rule.node, model, env, seen) : false;
  }
  if (node.kind === 'map') return node.entries.some(hasPreludeEntryKey);
  if (node.kind === 'choice' || node.kind === 'groupMerge') return node.options.some((option) => hasPreludeNameKey(option, model, env, seen));
  return false;
}

function hasPreludeEntryKey(entry) {
  if (!entry) return false;
  if (entry.kind === 'member') return entry.key.kind === 'name' && BUILTINS.has(entry.key.name);
  if (entry.kind === 'group') return entry.entries.some(hasPreludeEntryKey);
  if (entry.kind === 'groupChoice') return entry.options.some((entries) => entries.some(hasPreludeEntryKey));
  return false;
}

function markNotJsonRepresentable(ctx, reason) {
  if (!ctx || !ctx.nonRepReasons || ctx.nonRepReasons.has(reason)) return;
  ctx.nonRepReasons.add(reason);
  ctx.jsonRepresentable = false;
  ctx.warnings.push(`This schema uses ${reason}, which JSON cannot represent — generate and validate this one as CBOR.`);
}

function stringForKey(key) {
  const normalized = String(key || '').toLowerCase().replace(/[^a-z0-9]/g, '');
  if (normalized.includes('email')) return 'user@example.com';
  if (normalized.includes('url') || normalized.includes('uri') || normalized === 'href' || normalized.includes('regid')) return 'https://example.com';
  if (normalized.includes('uuid')) return '123e4567-e89b-12d3-a456-426614174000';
  if (normalized === 'id' || normalized.endsWith('id') || normalized.includes('identifier') || normalized.includes('jti')) return '123e4567-e89b-12d3-a456-426614174000';
  if (normalized.includes('date') || normalized.includes('time') || normalized === 'iat' || normalized === 'nbf' || normalized === 'exp' || normalized.includes('expire')) return '2026-08-17T16:56:23Z';
  if (normalized.includes('phone')) return '+1-555-0100';
  if (normalized.includes('name')) return 'example name';
  if (normalized.includes('address') || normalized.includes('street')) return '123 Example Street';
  if (normalized.includes('city')) return 'Example City';
  if (normalized.includes('country')) return 'Example Country';
  if (normalized.includes('host')) return 'example.com';
  if (normalized.includes('version')) return '1.0.0';
  if (normalized.includes('type') || normalized.includes('fmt')) return 'example type';
  return 'string';
}

function generateMap(entries, ctx, depth, visiting, env, target) {
  const object = target || {};
  for (const entry of entries) {
    if (!entry) continue;
    if (entry.kind === 'groupChoice') {
      ctx.warnings.push('Using first alternative of group choice.');
      mergeMembers(object, generateMap(entry.options[0], ctx, depth + 1, visiting, env));
      continue;
    }

    if (entry.kind === 'group') {
      if (skipOptional(entry, ctx)) continue;
      const count = repeatCount(entry.occurrence, ctx);
      // Repeated groups generate into the same object so dynamic keys stay distinct.
      for (let i = 0; i < count; i++) generateMap(entry.entries, ctx, depth + 1, visiting, env, object);
      continue;
    }
    if (entry.kind === 'member') {
      if (skipOptional(entry, ctx)) continue;
      const count = repeatCount(entry.occurrence, ctx);
      for (let i = 0; i < count; i++) {
        const baseKey = keyToString(entry.key, ctx, depth, visiting, env, entry.arrow);
        // A map cannot hold duplicate keys, so repeated members need distinct ones.
        const taken = Object.prototype.hasOwnProperty.call(object, baseKey);
        const key = i === 0 && !taken ? baseKey : distinctKey(object, baseKey, entry, ctx, i);
        const before = ctx.warnings.length;
        setMember(object, key, generateNode(entry.value, key, ctx, depth + 1, visiting, env, key));
        if (entry.occurrence.optional && ctx.warnings.length > before) {
          ctx.warnings.push(`Optional member "${key}" was included using a heuristic value.`);
        }
      }
      continue;
    }
    if (entry.kind === 'element') {
      const value = generateNode(entry.value, null, ctx, depth + 1, visiting, env, null);
      if (value && typeof value === 'object' && !Array.isArray(value)) mergeMembers(object, value);
      else if (value !== null) {
        ctx.warnings.push('Group entry did not produce an object member; stored it under "value".');
        setMember(object, 'value', value);
      }
    }
  }
  return object;
}

/** A key not already present, for repeated map members. Literal keys cannot repeat. */
function distinctKey(object, baseKey, entry, ctx, index) {
  if (entry.key?.kind === 'literal') {
    ctx.constraintsSatisfied = false;
    const repeats = entry.occurrence.raw ? `repeats ${entry.occurrence.raw} times` : 'occurs more than once';
    ctx.warnings.push(`Member "${baseKey}" ${repeats} but has a literal key; a JSON object cannot hold duplicates.`);
    return baseKey;
  }
  let candidate = `${baseKey}${index + 1}`;
  let suffix = index + 1;
  while (Object.prototype.hasOwnProperty.call(object, candidate)) {
    suffix += 1;
    candidate = `${baseKey}${suffix}`;
  }
  return candidate;
}

function generateGroupMerge(options, ctx, depth, visiting, env) {
  if (options.length > 1) ctx.warnings.push('Using first alternative of group socket choices.');
  const value = generateNode(options[0], null, ctx, depth + 1, visiting, env, null);
  return value && typeof value === 'object' && !Array.isArray(value) ? value : {};
}

function generateArray(entries, ctx, depth, visiting, env) {
  const array = [];
  for (const entry of entries) {
    if (!entry) continue;
    if (entry.kind === 'groupChoice') {
      ctx.warnings.push('Using first alternative of array group choice.');
      array.push(...generateArray(entry.options[0], ctx, depth + 1, visiting, env));
      continue;
    }
    if (skipOptional(entry, ctx)) continue;
    const count = repeatCount(entry.occurrence, ctx);
    for (let i = 0; i < count; i++) {
      if (entry.kind === 'member') array.push(generateNode(entry.value, keyToString(entry.key, ctx, depth, visiting, env, entry.arrow), ctx, depth + 1, visiting, env, null));
      else if (entry.kind === 'group') array.push(...generateArray(entry.entries, ctx, depth + 1, visiting, env));
      else {
        // A group reference contributes its entries to the array, not a nested array.
        const seen = new Set();
        const resolved = resolveGroupNode(entry.value, ctx, env, seen, visiting);
        if (resolved) array.push(...generateArray(resolved.group.entries, ctx, depth + 1, new Set([...visiting, ...seen]), resolved.env));
        else array.push(generateNode(entry.value, null, ctx, depth + 1, visiting, env, null));
      }
    }
  }
  return array;
}

/** True when entries describe map members rather than array elements. */
function hasMemberEntries(entries) {
  return (entries || []).some((entry) => {
    if (!entry) return false;
    if (entry.kind === 'member') return true;
    if (entry.kind === 'group') return hasMemberEntries(entry.entries);
    if (entry.kind === 'groupChoice') return entry.options.some(hasMemberEntries);
    return false;
  });
}

/** Follow references to an inline group definition with its environment, or null when it is not one. */
function resolveGroupNode(node, ctx, env, seen, visiting) {
  if (!node) return null;
  if (node.kind === 'group') return hasMemberEntries(node.entries) ? null : { group: node, env };
  if (node.kind !== 'ref') return null;
  if (env?.has(node.name)) return resolveGroupNode(env.get(node.name), ctx, env, seen, visiting);
  if (seen.has(node.name) || visiting?.has(node.name)) return null;
  seen.add(node.name);
  const rule = ctx.model.rules.get(node.name);
  if (!rule) return null;
  if (rule.params.length === 0) return resolveGroupNode(rule.node, ctx, env, seen, visiting);
  const nextEnv = new Map(env || []);
  rule.params.forEach((param, index) => {
    if (node.args?.[index]) nextEnv.set(param, node.args[index]);
  });
  return resolveGroupNode(rule.node, ctx, nextEnv, seen, visiting);
}

function generateEnum(entries, ctx, depth, visiting, env) {
  const first = entries.find(Boolean);
  if (!first) {
    ctx.warnings.push('Empty group-to-choice enumeration; emitted null.');
    return null;
  }
  if (first.kind === 'groupChoice') {
    ctx.warnings.push('Using first alternative of group-to-choice enumeration.');
    return generateEnum(first.options[0], ctx, depth + 1, visiting, env);
  }
  if (first.kind === 'member') return generateNode(first.value, keyToString(first.key, ctx, depth, visiting, env, first.arrow), ctx, depth + 1, visiting, env, null);
  return generateNode(first.value, null, ctx, depth + 1, visiting, env, null);
}

function keyToString(key, ctx, depth, visiting, env, arrow = false) {
  if (!key) return 'key';
  if (key.kind === 'literal') {
    if (typeof key.value !== 'string') markNotJsonRepresentable(ctx, 'integer or non-text member keys');
    return String(key.value);
  }
  if (key.kind === 'name' && arrow && (ctx.model.rules.has(key.name) || BUILTINS.has(key.name))) {
    const value = generateRef(key.name, [], null, ctx, depth + 1, visiting, env);
    if (typeof value !== 'string') markNotJsonRepresentable(ctx, 'integer or non-text member keys');
    return value == null ? key.name : String(value);
  }
  if (key.kind === 'name') return key.name;
  const value = generateNode(key.node, null, ctx, depth + 1, visiting, env, null);
  if (typeof value !== 'string') markNotJsonRepresentable(ctx, 'integer or non-text member keys');
  ctx.warnings.push(`Computed map key "${key.text}" approximated as a JSON object key.`);
  return value == null ? 'key' : String(value);
}

function collectReferences(model) {
  const refs = new Set();
  for (const rule of model.rules.values()) collectNodeReferences(rule.node, refs);
  return refs;
}

function collectNodeReferences(node, refs) {
  if (!node) return;
  if (node.kind === 'ref') {
    refs.add(node.name);
    node.args.forEach((arg) => collectNodeReferences(arg, refs));
    return;
  }
  for (const value of Object.values(node)) {
    if (Array.isArray(value)) value.forEach((item) => collectNodeReferences(item, refs));
    else if (value && typeof value === 'object') collectNodeReferences(value, refs);
  }
}

function describeTokens(tokens) {
  return tokens.map((token) => token.raw || token.value).join(' ').trim();
}
