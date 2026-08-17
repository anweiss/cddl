/**
 * @typedef {{ name: string, text: string|null, bytes: Uint8Array|null, kind: 'cddl'|'json'|'cbor'|'unknown' }} LoadedFile
 */

// ─── Classification ──────────────────────────────────────────────────────────

/** Classify a filename into a kind. */
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

/** Convert bytes to a lowercase hex string (used to show a dropped .cbor file in the editor). */
export function bytesToHex(bytes) {
  return Array.from(bytes || [], (byte) => byte.toString(16).padStart(2, '0')).join('');
}

// ─── Reading ─────────────────────────────────────────────────────────────────

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
 * @param {boolean|undefined} binary
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
        const bytes = new Uint8Array(reader.result || new ArrayBuffer(0));
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
      console.error('Failed to read file:', err);
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

    const settle = (value) => {
      if (settled) {
        return;
      }

      settled = true;
      cleanup();
      resolve(value);
    };

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
      console.error('Failed to open file picker:', err);
      settle(null);
    }
  });
}

// ─── Downloads ───────────────────────────────────────────────────────────────

function sanitizeFilename(filename) {
  const safeName = String(filename || 'download').replace(/[\\/\x00-\x1f\x7f]/g, '').trim();
  return safeName || 'download';
}

function downloadBlob(filename, blob) {
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');

  link.href = url;
  link.download = sanitizeFilename(filename);
  link.click();

  setTimeout(() => URL.revokeObjectURL(url), 0);
}

/** Trigger a download of text content. */
export function downloadText(filename, text, mimeType = 'text/plain;charset=utf-8') {
  downloadBlob(filename, new Blob([String(text ?? '')], { type: mimeType }));
}

/** Trigger a download of binary content. */
export function downloadBytes(filename, bytes, mimeType = 'application/octet-stream') {
  downloadBlob(filename, new Blob([bytes || new Uint8Array(0)], { type: mimeType }));
}

// ─── Drag And Drop ───────────────────────────────────────────────────────────

function dragHasFiles(event) {
  const types = event.dataTransfer && event.dataTransfer.types;
  return !!types && Array.from(types).includes('Files');
}

function showOverlay(overlayEl) {
  if (overlayEl) {
    overlayEl.classList.add('visible');
  }
}

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

  const onDragEnter = (event) => {
    if (!dragHasFiles(event)) {
      return;
    }

    event.preventDefault();
    depth += 1;
    showOverlay(overlayEl);
  };

  const onDragOver = (event) => {
    if (!dragHasFiles(event)) {
      return;
    }

    event.preventDefault();
    if (event.dataTransfer) {
      event.dataTransfer.dropEffect = 'copy';
    }
  };

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
