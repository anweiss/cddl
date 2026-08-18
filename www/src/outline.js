/**
 * @typedef {{ name: string, line: number, column: number, kind: 'type'|'group'|'socket'|'extension',
 *             generic: string|null, preview: string, comment: string|null }} CddlRule
 */

const IDENT_RE = /[A-Za-z0-9_.@$-]/;
const IDENT_START_RE = /[A-Za-z_@$]/;
const _escapeMap = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };
const _escapeRe = /[&<>"']/g;

function escapeHtml(str) {
  return String(str).replace(_escapeRe, (ch) => _escapeMap[ch]);
}

function isIdentChar(ch) {
  return IDENT_RE.test(ch);
}

function isIdentStart(ch) {
  return IDENT_START_RE.test(ch);
}

function lineStarts(source) {
  const starts = [0];
  for (let i = 0; i < source.length; i++) {
    if (source[i] === '\n') starts.push(i + 1);
  }
  return starts;
}

function positionFromIndex(starts, index) {
  let lo = 0;
  let hi = starts.length - 1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (starts[mid] <= index) lo = mid + 1;
    else hi = mid - 1;
  }
  return { line: hi + 1, column: index - starts[hi] + 1 };
}

function stripLineComment(line) {
  let quote = null;
  let escaped = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (quote) {
      if (escaped) escaped = false;
      else if (ch === '\\' && quote === '"') escaped = true;
      else if (ch === quote) quote = null;
    } else if (ch === '"' || ch === "'") {
      quote = ch;
    } else if (ch === ';') {
      return line.slice(0, i);
    }
  }
  return line;
}

function isCommentOnly(line) {
  return line.trimStart().startsWith(';');
}

function leadingComment(lines, ruleLineIndex) {
  const parts = [];
  for (let i = ruleLineIndex - 1; i >= 0; i--) {
    const line = lines[i];
    if (!isCommentOnly(line)) break;
    parts.unshift(line.trimStart().slice(1).trim());
  }
  return parts.length ? parts.join(' ') : null;
}

function skipSpacesAndComments(source, index) {
  let i = index;
  while (i < source.length) {
    const ch = source[i];
    if (ch === ' ' || ch === '\t' || ch === '\r' || ch === '\n') {
      i++;
    } else if (ch === ';') {
      while (i < source.length && source[i] !== '\n') i++;
    } else {
      break;
    }
  }
  return i;
}

function readGeneric(source, index) {
  if (source[index] !== '<') return { generic: null, end: index };
  let i = index;
  let depth = 0;
  let quote = null;
  let escaped = false;
  while (i < source.length) {
    const ch = source[i];
    if (quote) {
      if (escaped) escaped = false;
      else if (ch === '\\' && quote === '"') escaped = true;
      else if (ch === quote) quote = null;
    } else if (ch === '"' || ch === "'") {
      quote = ch;
    } else if (ch === ';') {
      while (i < source.length && source[i] !== '\n') i++;
      continue;
    } else if (ch === '<') {
      depth++;
    } else if (ch === '>') {
      depth--;
      if (depth === 0) return { generic: source.slice(index, i + 1), end: i + 1 };
    }
    i++;
  }
  return { generic: null, end: index };
}

function readAssignment(source, index) {
  if (source.startsWith('//=', index)) return { op: '//=', end: index + 3 };
  if (source.startsWith('/=', index)) return { op: '/=', end: index + 2 };
  if (
    source[index] === '=' &&
    source[index + 1] !== '>' &&
    source[index + 1] !== '=' &&
    source[index - 1] !== '!' &&
    source[index - 1] !== '/'
  ) {
    return { op: '=', end: index + 1 };
  }
  return null;
}

function endOfRule(source, index) {
  let i = index;
  let depth = 0;
  let quote = null;
  let escaped = false;
  while (i < source.length) {
    const ch = source[i];
    if (quote) {
      if (escaped) escaped = false;
      else if (ch === '\\' && quote === '"') escaped = true;
      else if (ch === quote) quote = null;
    } else if (ch === '"' || ch === "'") {
      quote = ch;
    } else if (ch === ';') {
      while (i < source.length && source[i] !== '\n') i++;
      continue;
    } else if (ch === '{' || ch === '[' || ch === '(') {
      depth++;
    } else if (ch === '}' || ch === ']' || ch === ')') {
      depth = Math.max(0, depth - 1);
    } else if (ch === '\n' && depth === 0) {
      let j = i + 1;
      while (j < source.length && (source[j] === ' ' || source[j] === '\t' || source[j] === '\r')) j++;
      if (j >= source.length || source[j] === '\n' || source[j] === ';') return i;
      if (isIdentStart(source[j]) || source[j] === '$') {
        const nameStart = j;
        while (j < source.length && isIdentChar(source[j])) j++;
        const generic = readGeneric(source, j);
        j = skipSpacesAndComments(source, generic.end);
        if (readAssignment(source, j)) return i;
      }
    }
    i++;
  }
  return source.length;
}

function makePreview(source, start, end) {
  const text = source
    .slice(start, end)
    .split('\n')
    .map(stripLineComment)
    .join(' ')
    .replace(/\s+/g, ' ')
    .trim();
  return text.length > 60 ? `${text.slice(0, 59)}…` : text;
}

function firstSignificant(source, index) {
  const i = skipSpacesAndComments(source, index);
  return source[i] || '';
}

/** Parse all top-level rule definitions from CDDL source. 1-based line/column. */
export function parseRules(source) {
  const text = String(source || '');
  const starts = lineStarts(text);
  const lines = text.split(/\r?\n/);
  const rules = [];
  let depth = 0;
  let quote = null;
  let escaped = false;
  let atLineStart = true;
  let lineHasOnlySpace = true;

  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (quote) {
      if (escaped) escaped = false;
      else if (ch === '\\' && quote === '"') escaped = true;
      else if (ch === quote) quote = null;
      if (ch === '\n') {
        atLineStart = true;
        lineHasOnlySpace = true;
      }
      continue;
    }

    if (ch === ';') {
      while (i < text.length && text[i] !== '\n') i++;
      atLineStart = true;
      lineHasOnlySpace = true;
      continue;
    }
    if (ch === '"' || ch === "'") {
      quote = ch;
      lineHasOnlySpace = false;
      continue;
    }
    if (ch === '\n') {
      atLineStart = true;
      lineHasOnlySpace = true;
      continue;
    }
    if (atLineStart && (ch === ' ' || ch === '\t' || ch === '\r')) continue;

    if (depth === 0 && atLineStart && lineHasOnlySpace && (isIdentStart(ch) || ch === '$')) {
      const nameStart = i;
      let j = i;
      while (j < text.length && isIdentChar(text[j])) j++;
      const name = text.slice(nameStart, j);
      const generic = readGeneric(text, j);
      j = skipSpacesAndComments(text, generic.end);
      const assignment = readAssignment(text, j);
      if (assignment) {
        const rhsStart = skipSpacesAndComments(text, assignment.end);
        const rhsEnd = endOfRule(text, rhsStart);
        const pos = positionFromIndex(starts, nameStart);
        const kind = name.startsWith('$')
          ? 'socket'
          : assignment.op !== '='
            ? 'extension'
            : firstSignificant(text, rhsStart) === '('
              ? 'group'
              : 'type';
        rules.push({
          name,
          line: pos.line,
          column: pos.column,
          kind,
          generic: generic.generic,
          preview: makePreview(text, rhsStart, rhsEnd),
          comment: leadingComment(lines, pos.line - 1),
        });
        i = Math.max(i, rhsEnd - 1);
        atLineStart = false;
        lineHasOnlySpace = false;
        continue;
      }
    }

    if (ch === '{' || ch === '[' || ch === '(') depth++;
    else if (ch === '}' || ch === ']' || ch === ')') depth = Math.max(0, depth - 1);
    atLineStart = false;
    lineHasOnlySpace = false;
  }
  return rules;
}

/** Find the rule defining `name`, or null. */
export function findRule(source, name) {
  return parseRules(source).find((rule) => rule.name === name) || null;
}

/**
 * Render the outline list into a container.
 * @param {HTMLElement} listEl
 * @param {CddlRule[]} rules
 * @param {{ filter?: string, onSelect?: (rule: CddlRule) => void, activeName?: string }} [opts]
 */
export function renderOutline(listEl, rules, opts = {}) {
  const filter = (opts.filter || '').toLowerCase();
  const visible = filter ? rules.filter((rule) => rule.name.toLowerCase().includes(filter)) : rules;
  const frag = document.createDocumentFragment();
  listEl.textContent = '';

  if (!visible.length) {
    const empty = document.createElement('div');
    empty.className = 'outline-empty';
    empty.textContent = 'No matching rules';
    frag.appendChild(empty);
    listEl.appendChild(frag);
    return;
  }

  const items = [];
  const activeIndex = visible.findIndex((rule) => rule.name === opts.activeName);
  const tabbableIndex = activeIndex >= 0 ? activeIndex : 0;

  const focusItem = (index) => {
    const next = items[index];
    if (!next) return;
    for (const item of items) item.tabIndex = -1;
    next.tabIndex = 0;
    next.focus();
  };

  visible.forEach((rule, index) => {
    const item = document.createElement('div');
    const isActive = opts.activeName === rule.name;
    item.className = `outline-item${isActive ? ' active' : ''}`;
    item.dataset.line = String(rule.line);
    item.dataset.column = String(rule.column);
    item.setAttribute('role', 'treeitem');
    item.setAttribute('aria-level', '1');
    item.setAttribute('aria-selected', isActive ? 'true' : 'false');
    item.tabIndex = index === tabbableIndex ? 0 : -1;
    item.innerHTML = `
      <span class="outline-item-kind ${escapeHtml(rule.kind)}">${escapeHtml(rule.kind)}</span>
      <span class="outline-item-name">${escapeHtml(rule.name)}${escapeHtml(rule.generic || '')}</span>
      <span class="outline-item-preview">${escapeHtml(rule.preview)}</span>
    `;
    const select = () => {
      if (opts.onSelect) opts.onSelect(rule);
    };
    item.addEventListener('click', select);
    item.addEventListener('keydown', (event) => {
      switch (event.key) {
        case 'Enter':
        case ' ':
          event.preventDefault();
          select();
          break;
        case 'ArrowDown':
          event.preventDefault();
          focusItem(Math.min(index + 1, items.length - 1));
          break;
        case 'ArrowUp':
          event.preventDefault();
          focusItem(Math.max(index - 1, 0));
          break;
        case 'Home':
          event.preventDefault();
          focusItem(0);
          break;
        case 'End':
          event.preventDefault();
          focusItem(items.length - 1);
          break;
        default:
          break;
      }
    });
    items.push(item);
    frag.appendChild(item);
  });
  listEl.appendChild(frag);
}

function cddlWordAt(model, position) {
  const line = model.getLineContent(position.lineNumber);
  let start = position.column - 1;
  let end = position.column - 1;
  while (start > 0 && isIdentChar(line[start - 1])) start--;
  while (end < line.length && isIdentChar(line[end])) end++;
  const word = line.slice(start, end);
  return word ? { word, startColumn: start + 1, endColumn: end + 1 } : model.getWordAtPosition(position);
}

function definitionLine(source, rule) {
  return (source.split(/\r?\n/)[rule.line - 1] || '').trimEnd();
}

/**
 * Register go-to-definition + hover providers for the 'cddl' language.
 * @param {object} monaco the monaco namespace
 * @param {() => string} getSource
 * @returns {() => void} disposer that disposes both providers
 */
export function registerNavigation(monaco, getSource) {
  const definitionProvider = monaco.languages.registerDefinitionProvider('cddl', {
    provideDefinition(model, position) {
      const word = cddlWordAt(model, position);
      if (!word || !word.word) return null;
      const rule = findRule(getSource(), word.word);
      if (!rule) return null;
      return {
        uri: model.uri,
        range: {
          startLineNumber: rule.line,
          startColumn: rule.column,
          endLineNumber: rule.line,
          endColumn: rule.column + rule.name.length,
        },
      };
    },
  });
  const hoverProvider = monaco.languages.registerHoverProvider('cddl', {
    provideHover(model, position) {
      const word = cddlWordAt(model, position);
      if (!word || !word.word) return null;
      const source = getSource();
      const rule = findRule(source, word.word);
      if (!rule) return null;
      const contents = [];
      if (rule.comment) contents.push({ value: rule.comment });
      contents.push({ value: `\`\`\`cddl\n${definitionLine(source, rule)}\n\`\`\`` });
      return {
        range: {
          startLineNumber: position.lineNumber,
          startColumn: word.startColumn,
          endLineNumber: position.lineNumber,
          endColumn: word.endColumn,
        },
        contents,
      };
    },
  });
  return () => {
    definitionProvider.dispose();
    hoverProvider.dispose();
  };
}
