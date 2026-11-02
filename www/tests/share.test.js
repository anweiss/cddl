// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  buildShareUrl,
  readStateFromLocation,
  clearLocationState,
  copyToClipboard,
  isTooLargeToShare,
} from '../src/share.js';

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.body.replaceChildren();
  history.replaceState(null, '', '/');
});

describe('sharing untrusted state', () => {
  it.each([
    { cddl: 'root = tstr' },
    { cddl: 'root = "héllo 世界"', instance: '"héllo 世界"', kind: 'json' },
    { cddl: 'root = uint', instance: '01', kind: 'cbor' },
  ])('round trips UTF-8 state %j without sending it to a server', (state) => {
    const link = buildShareUrl(state, 'https://example.test/play?mode=1#old');
    expect(link).toMatch(/^https:\/\/example\.test\/play\?mode=1#s1=/);
    vi.stubGlobal('location', new URL(link));
    expect(readStateFromLocation()).toEqual(state);
    expect(isTooLargeToShare(state)).toBe(false);
  });

  it.each(['#', '#other=1', '#s1=', '#s1=%%%', '#s1=_w', '#s1=YQ', '#s1=' + 'a'.repeat(32001)])(
    'rejects invalid or oversized fragment %s',
    (hash) => {
      vi.stubGlobal('location', { hash });
      expect(readStateFromLocation()).toBeNull();
    },
  );
  it.each([[], {}, { c: 1 }, { c: 'root = uint', i: 1, k: 'invalid' }])(
    'validates decoded payload shape %j',
    (payload) => {
      vi.stubGlobal('location', { hash: '#s1=' + btoa(JSON.stringify(payload)) });
      expect(readStateFromLocation()).toEqual(
        typeof payload.c === 'string' ? { cddl: payload.c } : null,
      );
    },
  );
  it('checks encoded bytes, not just character count', () => {
    expect(isTooLargeToShare({ cddl: '界'.repeat(9000) })).toBe(true);
  });
  it('clears the fragment while retaining path and query', () => {
    history.replaceState(null, '', '/play?mode=1#s1=abc');
    clearLocationState();
    expect(location.href).not.toContain('#');
    expect(location.pathname + location.search).toBe('/play?mode=1');
  });
  it('copies using the clipboard API, with a cleaned-up fallback', async () => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    vi.stubGlobal('navigator', { clipboard: { writeText } });
    expect(await copyToClipboard('schema')).toBe(true);
    expect(writeText).toHaveBeenCalledWith('schema');
    writeText.mockRejectedValue(new Error('denied'));
    document.execCommand = vi.fn().mockReturnValue(true);
    expect(await copyToClipboard('fallback')).toBe(true);
    expect(document.querySelector('textarea')).toBeNull();
    document.execCommand = vi.fn(() => {
      throw new Error('denied');
    });
    expect(await copyToClipboard('blocked')).toBe(false);
    expect(document.querySelector('textarea')).toBeNull();
  });
});
