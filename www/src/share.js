/**
 * @typedef {{ cddl: string, instance?: string, kind?: 'json'|'cbor' }} PlaygroundState
 */

export const MAX_SHARE_BYTES = 32000;

const SHARE_PREFIX = 's1=';

// ─── UTF-8 Base64url ─────────────────────────────────────────────────────────

function bytesToBase64(bytes) {
  let binary = '';
  for (let i = 0; i < bytes.length; i += 1) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary);
}

function base64ToBytes(base64) {
  const binary = atob(base64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

function toBase64Url(text) {
  const bytes = new TextEncoder().encode(text);
  return bytesToBase64(bytes)
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/g, '');
}

function fromBase64Url(payload) {
  if (!/^[A-Za-z0-9_-]+$/.test(payload)) {
    return null;
  }

  const padding = (4 - (payload.length % 4)) % 4;
  const base64 = `${payload.replace(/-/g, '+').replace(/_/g, '/')}${'='.repeat(padding)}`;
  const bytes = base64ToBytes(base64);
  return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
}

function compactState(state) {
  const compact = {
    c: typeof state?.cddl === 'string' ? state.cddl : '',
  };

  if (typeof state?.instance === 'string' && state.instance.length > 0) {
    compact.i = state.instance;
  }

  if (state?.kind === 'json' || state?.kind === 'cbor') {
    compact.k = state.kind;
  }

  return compact;
}

function expandState(compact) {
  if (!compact || typeof compact !== 'object' || Array.isArray(compact)) {
    return null;
  }

  if (typeof compact.c !== 'string') {
    return null;
  }

  const state = { cddl: compact.c };

  if (typeof compact.i === 'string' && compact.i.length > 0) {
    state.instance = compact.i;
  }

  if (compact.k === 'json' || compact.k === 'cbor') {
    state.kind = compact.k;
  }

  return state;
}

// ─── Share State ─────────────────────────────────────────────────────────────

/** Encode state into a URL-hash-safe string (no leading '#'). */
export function encodeState(state) {
  return `${SHARE_PREFIX}${toBase64Url(JSON.stringify(compactState(state)))}`;
}

/** Decode a hash string (with or without leading '#'); returns null if absent/invalid. */
export function decodeState(hash) {
  try {
    const fragment = typeof hash === 'string' && hash.startsWith('#') ? hash.slice(1) : hash;
    if (typeof fragment !== 'string' || !fragment.startsWith(SHARE_PREFIX)) {
      return null;
    }

    const payload = fragment.slice(SHARE_PREFIX.length);
    if (!payload) {
      return null;
    }

    const json = fromBase64Url(payload);
    if (json === null) {
      return null;
    }

    return expandState(JSON.parse(json));
  } catch (err) {
    return null;
  }
}

/** Build a full absolute shareable URL for the given state. */
export function buildShareUrl(state, baseUrl) {
  let url = baseUrl;
  try {
    if (typeof url !== 'string') {
      url = globalThis.location?.href || '';
    }
    url = url.split('#')[0];
  } catch (err) {
    url = '';
  }

  return `${url}#${encodeState(state)}`;
}

export function isTooLargeToShare(state) {
  return encodeState(state).length > MAX_SHARE_BYTES;
}

/** Read state from the current window.location. Returns null if no share payload. */
export function readStateFromLocation() {
  try {
    return decodeState(globalThis.location?.hash || '');
  } catch (err) {
    return null;
  }
}

/** Remove the share payload from the address bar without reloading (history.replaceState). */
export function clearLocationState() {
  try {
    const location = globalThis.location;
    const history = globalThis.history;
    if (!location || !history || typeof history.replaceState !== 'function') {
      return;
    }

    history.replaceState(null, '', `${location.pathname || ''}${location.search || ''}`);
  } catch (err) {
    // Ignore browsers that disallow history updates.
  }
}

/** Copy text to clipboard with a execCommand fallback. @returns {Promise<boolean>} success */
export async function copyToClipboard(text) {
  try {
    if (globalThis.navigator?.clipboard?.writeText) {
      await globalThis.navigator.clipboard.writeText(text);
      return true;
    }
  } catch (err) {
    // Fall back below.
  }

  let textarea;
  try {
    const document = globalThis.document;
    if (!document?.body || typeof document.createElement !== 'function') {
      return false;
    }

    textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.setAttribute('readonly', '');
    textarea.style.position = 'fixed';
    textarea.style.left = '-9999px';
    textarea.style.top = '0';
    document.body.appendChild(textarea);
    textarea.select();
    return document.execCommand('copy') === true;
  } catch (err) {
    return false;
  } finally {
    try {
      if (textarea?.parentNode) {
        textarea.parentNode.removeChild(textarea);
      }
    } catch (err) {
      // Ignore cleanup failures.
    }
  }
}
