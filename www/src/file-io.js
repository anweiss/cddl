/**
 * @typedef {{ name: string, text: string|null, bytes: Uint8Array|null, kind: 'cddl'|'json'|'cbor'|'unknown' }} LoadedFile
 */
import { logFailure } from './logger.js';

// ─── Classification ──────────────────────────────────────────────────────────

/** @type {(name: string) => LoadedFile['kind']} Classify a filename. */
export function classifyFile(name) {
  const lowerName = String(name || '').toLowerCase();

  if (lowerName.endsWith('.cddl')) {
    return 'cddl';
  }
  if (lowerName.endsWith('.json')) {
    return 'json';
  }
  if (lowerName.endsWith('.cbor')) {
    return 'cbor';
  }

  return 'unknown';
}

/** @type {(bytes: Uint8Array) => string} Lowercase hex for the CBOR editor. */
export function bytesToHex(bytes) {
  return Array.from(bytes || [], (byte) => byte.toString(16).padStart(2, '0')).join('');
}

// ─── Reading ─────────────────────────────────────────────────────────────────

/** @type {(file: File, binary?: boolean) => boolean} */
function shouldReadBinary(file, binary) {
  if (typeof binary === 'boolean') {
    return binary;
  }

  return classifyFile(file.name) === 'cbor';
}

/**
 * Read a File into the LoadedFile shape. FileReader errors resolve to null
 * instead of rejecting so callers can show one consistent user-facing message.
 * @param {File} file
 * @param {boolean} [binary]
 * @returns {Promise<LoadedFile|null>}
 */
function readLoadedFile(file, binary) {
  const kind = classifyFile(file.name);
  const readBinary = shouldReadBinary(file, binary);

  return new Promise((resolve) => {
    const reader = new FileReader();

    reader.onerror = () => resolve(null);
    reader.onabort = () => resolve(null);
    reader.onload = () => {
      if (readBinary) {
        const bytes = new Uint8Array(
          reader.result instanceof ArrayBuffer ? reader.result : new ArrayBuffer(0),
        );
        resolve({
          name: file.name,
          text: bytesToHex(bytes),
          bytes,
          kind,
        });
        return;
      }

      resolve({
        name: file.name,
        text: String(reader.result || ''),
        bytes: null,
        kind,
      });
    };

    try {
      if (readBinary) {
        reader.readAsArrayBuffer(file);
      } else {
        reader.readAsText(file);
      }
    } catch (err) {
      logFailure('file.read_failed', err);
      resolve(null);
    }
  });
}

// ─── Picker ──────────────────────────────────────────────────────────────────

/**
 * Open the native file picker. Resolves null if the user cancels. FileReader
 * errors also resolve null instead of rejecting so callers can show a toast.
 * @param {{ accept?: string, binary?: boolean }} [opts]
 * @returns {Promise<LoadedFile|null>}
 */
export function pickFile(opts = {}) {
  return new Promise((resolve) => {
    if (typeof document === 'undefined' || typeof window === 'undefined') {
      resolve(null);
      return;
    }

    const input = document.createElement('input');
    let settled = false;

    input.type = 'file';
    if (opts.accept) {
      input.accept = opts.accept;
    }

    const cleanup = () => {
      input.removeEventListener('change', onChange);
      input.removeEventListener('cancel', onCancel);
      window.removeEventListener('focus', onWindowFocus);
      input.remove();
    };

    /** @param {LoadedFile | null} value */
    const settle = (value) => {
      if (settled) {
        return;
      }

      settled = true;
      cleanup();
      resolve(value);
    };

    /** @param {File} file */
    const readAndSettle = async (file) => {
      settle(await readLoadedFile(file, opts.binary));
    };

    function onChange() {
      const file = input.files && input.files[0];
      if (!file) {
        settle(null);
        return;
      }

      readAndSettle(file);
    }

    function onCancel() {
      settle(null);
    }

    function onWindowFocus() {
      setTimeout(() => {
        if (!settled && (!input.files || input.files.length === 0)) {
          settle(null);
        }
      }, 0);
    }

    input.addEventListener('change', onChange);
    input.addEventListener('cancel', onCancel);
    window.addEventListener('focus', onWindowFocus);

    try {
      input.click();
    } catch (err) {
      logFailure('file.picker_failed', err);
      settle(null);
    }
  });
}

// ─── Downloads ───────────────────────────────────────────────────────────────

/** @type {(filename: string) => string} */
function sanitizeFilename(filename) {
  const safeName = Array.from(String(filename || 'download'))
    .filter(
      (char) =>
        char !== '/' && char !== '\\' && char.charCodeAt(0) >= 32 && char.charCodeAt(0) !== 127,
    )
    .join('')
    .trim();
  return safeName || 'download';
}

/** @type {(filename: string, blob: Blob) => void} */
function downloadBlob(filename, blob) {
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');

  link.href = url;
  link.download = sanitizeFilename(filename);
  link.click();

  setTimeout(() => URL.revokeObjectURL(url), 0);
}

/** @type {(filename: string, text: string, mimeType?: string) => void} Download text. */
export function downloadText(filename, text, mimeType = 'text/plain;charset=utf-8') {
  downloadBlob(filename, new Blob([String(text ?? '')], { type: mimeType }));
}

// ─── Drag And Drop ───────────────────────────────────────────────────────────

/** @type {(event: DragEvent) => boolean} */
function dragHasFiles(event) {
  const types = event.dataTransfer && event.dataTransfer.types;
  return !!types && Array.from(types).includes('Files');
}

/** @type {(overlayEl: HTMLElement | null) => void} */
function showOverlay(overlayEl) {
  if (overlayEl) {
    overlayEl.classList.add('visible');
  }
}

/** @type {(overlayEl: HTMLElement | null) => void} */
function hideOverlay(overlayEl) {
  if (overlayEl) {
    overlayEl.classList.remove('visible');
  }
}

/**
 * Wire drag-and-drop on an element.
 * @param {HTMLElement} target        element to listen on (usually document.body)
 * @param {HTMLElement|null} overlayEl element to show/hide via the 'visible' class during a drag
 * @param {(file: LoadedFile) => void} onFile  called once per dropped file
 * @returns {() => void} a teardown function that removes all listeners
 */
export function attachDropZone(target, overlayEl, onFile) {
  let depth = 0;

  const reset = () => {
    depth = 0;
    hideOverlay(overlayEl);
  };

  /** @param {DragEvent} event */
  const onDragEnter = (event) => {
    if (!dragHasFiles(event)) {
      return;
    }

    event.preventDefault();
    depth += 1;
    showOverlay(overlayEl);
  };

  /** @param {DragEvent} event */
  const onDragOver = (event) => {
    if (!dragHasFiles(event)) {
      return;
    }

    event.preventDefault();
    if (event.dataTransfer) {
      event.dataTransfer.dropEffect = 'copy';
    }
  };

  /** @param {DragEvent} event */
  const onDragLeave = (event) => {
    if (!dragHasFiles(event)) {
      return;
    }

    event.preventDefault();
    depth = Math.max(0, depth - 1);
    if (depth === 0) {
      hideOverlay(overlayEl);
    }
  };

  /** @param {DragEvent} event */
  const onDrop = (event) => {
    if (!dragHasFiles(event)) {
      return;
    }

    event.preventDefault();
    reset();

    const files = Array.from((event.dataTransfer && event.dataTransfer.files) || []);
    for (const file of files) {
      readLoadedFile(file).then((loadedFile) => {
        if (loadedFile) {
          onFile(loadedFile);
        }
      });
    }
  };

  target.addEventListener('dragenter', onDragEnter);
  target.addEventListener('dragover', onDragOver);
  target.addEventListener('dragleave', onDragLeave);
  target.addEventListener('drop', onDrop);
  target.addEventListener('dragend', reset);

  return () => {
    target.removeEventListener('dragenter', onDragEnter);
    target.removeEventListener('dragover', onDragOver);
    target.removeEventListener('dragleave', onDragLeave);
    target.removeEventListener('drop', onDrop);
    target.removeEventListener('dragend', reset);
    reset();
  };
}
