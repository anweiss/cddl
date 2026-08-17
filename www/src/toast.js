const MAX_TOASTS = 4;
const DEFAULT_DURATION_MS = 3200;
const TRANSITION_FALLBACK_MS = 250;

let containerEl = null;
let warnedMissingContainer = false;
let toasts = [];

const ICONS = {
  info: 'ℹ',
  success: '✓',
  error: '!',
  warning: '⚠',
};

function warnMissingContainer() {
  if (warnedMissingContainer) return;
  warnedMissingContainer = true;
  if (typeof console !== 'undefined' && typeof console.warn === 'function') {
    console.warn('Toasts have not been initialized. Call initToasts(containerEl) before toast().');
  }
}

function normalizeType(type) {
  return Object.prototype.hasOwnProperty.call(ICONS, type) ? type : 'info';
}

function removeToastRecord(record) {
  toasts = toasts.filter((toastRecord) => toastRecord !== record);
}

function scheduleTimer(record, durationMs) {
  if (record.timerId) clearTimeout(record.timerId);
  record.timerId = null;
  if (durationMs > 0) {
    record.timerId = setTimeout(record.dismiss, durationMs);
  }
}

function removeToastNode(record) {
  if (record.removed) return;
  record.removed = true;
  if (record.fallbackTimerId) clearTimeout(record.fallbackTimerId);
  if (record.el.parentNode) {
    record.el.parentNode.removeChild(record.el);
  }
}

function createToast(doc, message, type) {
  const el = doc.createElement('div');
  el.className = `toast toast-${type}`;
  if (type === 'error') {
    el.setAttribute('role', 'alert');
  }

  const iconEl = doc.createElement('span');
  iconEl.className = 'toast-icon';
  iconEl.setAttribute('aria-hidden', 'true');
  iconEl.textContent = ICONS[type];
  el.appendChild(iconEl);

  const messageEl = doc.createElement('span');
  messageEl.className = 'toast-message';
  messageEl.textContent = message;
  el.appendChild(messageEl);

  const closeBtn = doc.createElement('button');
  closeBtn.className = 'toast-close';
  closeBtn.type = 'button';
  closeBtn.setAttribute('aria-label', 'Dismiss');
  closeBtn.textContent = '×';
  el.appendChild(closeBtn);

  return { el, closeBtn };
}

/** Set the container element the toasts render into. Must be called once at boot. */
export function initToasts(container) {
  containerEl = container || null;
}

/**
 * Show a toast.
 * @param {string} message
 * @param {'info'|'success'|'error'|'warning'} [type='info']
 * @param {number} [durationMs=3200] pass 0 to make it sticky until dismissed
 * @returns {() => void} a dismiss function
 */
export function toast(message, type = 'info', durationMs = DEFAULT_DURATION_MS) {
  if (!containerEl) {
    warnMissingContainer();
    return () => {};
  }

  const normalizedType = normalizeType(type);
  const normalizedMessage = String(message);
  const existing = toasts.find((record) => (
    !record.removing &&
    record.message === normalizedMessage &&
    record.type === normalizedType
  ));
  if (existing) {
    scheduleTimer(existing, durationMs);
    return existing.dismiss;
  }

  const doc = containerEl.ownerDocument || (typeof document !== 'undefined' ? document : null);
  if (!doc) {
    warnMissingContainer();
    return () => {};
  }

  const { el, closeBtn } = createToast(doc, normalizedMessage, normalizedType);
  const record = {
    el,
    message: normalizedMessage,
    type: normalizedType,
    timerId: null,
    fallbackTimerId: null,
    removing: false,
    removed: false,
    dismiss: null,
  };

  record.dismiss = () => {
    if (record.removing) return;
    record.removing = true;
    if (record.timerId) clearTimeout(record.timerId);
    record.timerId = null;
    removeToastRecord(record);

    const onTransitionEnd = (e) => {
      if (e && e.target !== el) return;
      el.removeEventListener('transitionend', onTransitionEnd);
      removeToastNode(record);
    };

    el.addEventListener('transitionend', onTransitionEnd);
    el.classList.remove('visible');
    record.fallbackTimerId = setTimeout(() => {
      el.removeEventListener('transitionend', onTransitionEnd);
      removeToastNode(record);
    }, TRANSITION_FALLBACK_MS);
  };

  closeBtn.addEventListener('click', record.dismiss);
  containerEl.appendChild(el);
  toasts.push(record);
  scheduleTimer(record, durationMs);

  while (toasts.length > MAX_TOASTS) {
    toasts[0].dismiss();
  }

  const show = () => {
    if (!record.removing) el.classList.add('visible');
  };
  if (typeof requestAnimationFrame === 'function') {
    requestAnimationFrame(show);
  } else {
    setTimeout(show, 0);
  }

  return record.dismiss;
}

/** Remove all visible toasts immediately. */
export function clearToasts() {
  for (const record of [...toasts]) {
    if (record.timerId) clearTimeout(record.timerId);
    if (record.fallbackTimerId) clearTimeout(record.fallbackTimerId);
    record.removing = true;
    removeToastNode(record);
  }
  toasts = [];
}
