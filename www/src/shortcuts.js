const ALLOWED_GROUPS = ['Editor', 'Playground', 'Panels'];

/** @typedef {{ keys: string[], desc: string, group: string }} Shortcut */

/** The canonical shortcut list, grouped. */
export const SHORTCUTS = Object.freeze([
  { keys: ['Mod', 'S'], desc: 'Format document (when Format on save is enabled)', group: 'Editor' },
  { keys: ['Mod', 'F'], desc: 'Find', group: 'Editor' },
  { keys: ['Mod', 'H'], desc: 'Replace', group: 'Editor' },
  { keys: ['Mod', '/'], desc: 'Toggle line comment', group: 'Editor' },
  { keys: ['Mod', 'Enter'], desc: 'Validate instance against schema', group: 'Playground' },
  { keys: ['Mod', 'K'], desc: 'Open the examples menu', group: 'Playground' },
  { keys: ['Mod', 'O'], desc: 'Open a file', group: 'Playground' },
  { keys: ['Mod', 'Shift', 'S'], desc: 'Download the schema', group: 'Playground' },
  { keys: ['Mod', 'Shift', 'C'], desc: 'Copy a shareable link', group: 'Playground' },
  { keys: ['?'], desc: 'Show this shortcuts help', group: 'Playground' },
  { keys: ['Esc'], desc: 'Close the open dialog/menu', group: 'Playground' },
  { keys: ['Mod', 'B'], desc: 'Toggle the outline sidebar', group: 'Panels' },
  { keys: ['Mod', 'I'], desc: 'Toggle the instance pane', group: 'Panels' },
  { keys: ['Mod', 'J'], desc: 'Toggle the problems panel', group: 'Panels' },
]);

let warnedMissingElements = false;

/** True on macOS — used to render ⌘ vs Ctrl. */
export function isMac() {
  if (typeof navigator === 'undefined') return false;
  const platform = navigator.userAgentData?.platform || navigator.platform || '';
  return /mac/i.test(platform);
}

function keyLabel(key) {
  if (key === 'Mod') return isMac() ? '⌘' : 'Ctrl';
  return key;
}

function getDocument(el) {
  if (el && el.ownerDocument) return el.ownerDocument;
  if (typeof document !== 'undefined') return document;
  return null;
}

function clearElement(el) {
  if ('textContent' in el) {
    el.textContent = '';
  }
  while (el.firstChild) {
    el.removeChild(el.firstChild);
  }
}

function appendShortcutRow(doc, parent, shortcut) {
  const rowEl = doc.createElement('div');
  rowEl.className = 'shortcut-row';

  const keysEl = doc.createElement('div');
  keysEl.className = 'shortcut-keys';
  for (const key of shortcut.keys) {
    const keyEl = doc.createElement('kbd');
    keyEl.className = 'shortcut-key';
    keyEl.textContent = keyLabel(key);
    keysEl.appendChild(keyEl);
  }
  rowEl.appendChild(keysEl);

  const descEl = doc.createElement('div');
  descEl.className = 'shortcut-desc';
  descEl.textContent = shortcut.desc;
  rowEl.appendChild(descEl);

  parent.appendChild(rowEl);
}

/** Render the shortcut list into the modal body. */
export function renderShortcuts(bodyEl) {
  if (!bodyEl) return;
  const doc = getDocument(bodyEl);
  if (!doc) return;

  clearElement(bodyEl);
  const frag = doc.createDocumentFragment();

  for (const group of ALLOWED_GROUPS) {
    const groupShortcuts = SHORTCUTS.filter((shortcut) => shortcut.group === group);
    if (groupShortcuts.length === 0) continue;

    const titleEl = doc.createElement('div');
    titleEl.className = 'shortcuts-group-title';
    titleEl.textContent = group;
    frag.appendChild(titleEl);

    for (const shortcut of groupShortcuts) {
      appendShortcutRow(doc, frag, shortcut);
    }
  }

  bodyEl.appendChild(frag);
}

function warnMissingElements() {
  if (warnedMissingElements) return;
  warnedMissingElements = true;
  if (typeof console !== 'undefined' && typeof console.warn === 'function') {
    console.warn('Shortcuts modal was not initialized because one or more elements are missing.');
  }
}

function noopController() {
  return {
    open: () => {},
    close: () => {},
    toggle: () => {},
    dispose: () => {},
  };
}

function isTypingTarget(target) {
  if (!target) return false;
  const tagName = target.tagName ? target.tagName.toLowerCase() : '';
  if (tagName === 'input' || tagName === 'textarea' || tagName === 'select') return true;
  if (target.isContentEditable) return true;
  if (typeof target.closest === 'function') {
    return Boolean(target.closest('[contenteditable], .monaco-editor'));
  }
  return false;
}

function focusableElements(modal) {
  if (typeof modal.querySelectorAll !== 'function') return [];
  return Array.from(modal.querySelectorAll([
    'a[href]',
    'button:not([disabled])',
    'input:not([disabled])',
    'select:not([disabled])',
    'textarea:not([disabled])',
    '[tabindex]:not([tabindex="-1"])',
  ].join(','))).filter((el) => (
    !el.hasAttribute ||
    (!el.hasAttribute('disabled') && el.getAttribute('aria-hidden') !== 'true')
  ));
}

/**
 * Wire the shortcuts modal open/close behavior.
 * @param {{ btn: HTMLElement, modal: HTMLElement, closeBtn: HTMLElement, bodyEl: HTMLElement }} els
 * @returns {{ open: () => void, close: () => void, toggle: () => void, dispose: () => void }}
 */
export function initShortcutsModal(els) {
  if (!els || !els.btn || !els.modal || !els.closeBtn || !els.bodyEl) {
    warnMissingElements();
    return noopController();
  }

  const { btn, modal, closeBtn, bodyEl } = els;
  const doc = getDocument(modal);
  if (!doc) {
    warnMissingElements();
    return noopController();
  }

  let open = false;
  let previousFocus = null;

  const controller = {
    open: () => {
      if (open) return;
      previousFocus = doc.activeElement;
      renderShortcuts(bodyEl);
      modal.classList.add('visible');
      modal.setAttribute('aria-hidden', 'false');
      open = true;
      if (typeof closeBtn.focus === 'function') closeBtn.focus();
    },
    close: () => {
      if (!open) return;
      modal.classList.remove('visible');
      modal.setAttribute('aria-hidden', 'true');
      open = false;
      if (previousFocus && typeof previousFocus.focus === 'function') {
        previousFocus.focus();
      }
      previousFocus = null;
    },
    toggle: () => {
      if (open) {
        controller.close();
      } else {
        controller.open();
      }
    },
    dispose: () => {
      btn.removeEventListener('click', onButtonClick);
      closeBtn.removeEventListener('click', onCloseClick);
      modal.removeEventListener('click', onBackdropClick);
      doc.removeEventListener('keydown', onKeyDown);
    },
  };

  const onButtonClick = () => controller.open();
  const onCloseClick = () => controller.close();
  const onBackdropClick = (e) => {
    if (e.target === modal) controller.close();
  };
  const onKeyDown = (e) => {
    if (e.key === '?' && !open && !isTypingTarget(doc.activeElement)) {
      e.preventDefault();
      controller.open();
      return;
    }

    if (!open) return;

    if (e.key === 'Escape' || e.key === 'Esc') {
      e.preventDefault();
      controller.close();
      return;
    }

    if (e.key !== 'Tab') return;

    const focusable = focusableElements(modal);
    if (focusable.length === 0) {
      e.preventDefault();
      closeBtn.focus();
      return;
    }

    const first = focusable[0];
    const last = focusable[focusable.length - 1];
    if (e.shiftKey && doc.activeElement === first) {
      e.preventDefault();
      last.focus();
    } else if (!e.shiftKey && doc.activeElement === last) {
      e.preventDefault();
      first.focus();
    }
  };

  btn.addEventListener('click', onButtonClick);
  closeBtn.addEventListener('click', onCloseClick);
  modal.addEventListener('click', onBackdropClick);
  doc.addEventListener('keydown', onKeyDown);
  modal.setAttribute('aria-hidden', 'true');

  return controller;
}
