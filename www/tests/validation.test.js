import { describe, expect, it, vi } from 'vitest';
import { isCborSupported, validateInstance } from '../src/instance-validation.js';
import { wasmModule } from './wasm.js';

// This is the actual Rust validator with the browser's default features.
const validate = (options) =>
  validateInstance({ wasmModule, cddl: 'root = uint', kind: 'json', text: '1', ...options });

describe('real WASM instance validation', () => {
  it.each(['0', '1', '18446744073709551615'])('accepts uint %s', (text) => {
    expect(validate({ text }).ok).toBe(true);
  });
  it.each(['-1', '"text"', 'true', '{}'])('rejects non-uint %s', (text) => {
    const result = validate({ text });
    expect(result.ok).toBe(false);
    expect(result.title).toContain('does not conform');
    expect(result.failures.length).toBeGreaterThan(0);
  });
  it.each(['01', '0x01', 'AQ==', 'AQ'])('decodes CBOR %s', (text) => {
    expect(validate({ kind: 'cbor', text }).ok).toBe(true);
  });
  it('accepts hex separators but rejects trailing CBOR items', () => {
    expect(
      validate({ kind: 'cbor', cddl: 'root = [uint, uint]', text: '0x82 : 0x01 : 0x02' }).ok,
    ).toBe(true);
    expect(validate({ kind: 'cbor', text: '01 02' }).title).toBe('Invalid CBOR input');
  });
  it.each([
    ['root = { count: uint }', '{"count":-1}'],
    ['root = [uint]', '[-1]'],
    ['root = alias\nalias = uint', '-1'],
  ])('rejects negative uint inside %s', (cddl, text) => {
    expect(validate({ cddl, text }).ok).toBe(false);
  });
  it.each(['', 'a', '!!!', 'A===', 'AQ=A', 'ff'])(
    'rejects empty, malformed or invalid CBOR %s',
    (text) => {
      expect(validate({ kind: 'cbor', text }).ok).toBe(false);
    },
  );
  it('validates against a selected non-first rule', () => {
    const cddl = 'first = uint\nsecond = tstr';
    expect(validate({ cddl, rootRule: 'second', text: '"hello"' }).ok).toBe(true);
    expect(validate({ cddl, text: '"hello"' }).ok).toBe(false);
  });
  it.each(['generic', 'group'])('rejects unsupported %s roots', (rootRule) => {
    const result = validate({
      cddl: 'root = uint\ngeneric<T> = [T]\ngroup = (x: uint)',
      rootRule,
    });
    expect(result.title).toBe('Unsupported root rule');
  });
  it('reports schema syntax errors and JSON syntax errors separately', () => {
    expect(validate({ cddl: 'root = {' }).title).toContain('parse');
    expect(validate({ text: '{' }).title).toContain('Invalid JSON');
    expect(validate({ cddl: '' }).title).toBe('Schema is empty');
    expect(validate({ text: '' }).title).toBe('Nothing to validate');
  });
  it('fails closed when WASM is unavailable', () => {
    expect(validate({ wasmModule: undefined }).title).toBe('WASM module not ready');
    expect(validate({ wasmModule: {}, kind: 'cbor' }).ok).toBe(false);
    expect(isCborSupported(wasmModule)).toBe(true);
    expect(isCborSupported(undefined)).toBe(false);
  });
  it('supports legacy validator signatures without hiding constraint failures', () => {
    const legacy = vi.fn((...args) => {
      if (args.length === 3) throw new TypeError('legacy arity');
    });
    expect(validate({ wasmModule: { validate_json_from_str: legacy } }).ok).toBe(true);
    expect(legacy).toHaveBeenCalledTimes(2);
    const broken = vi.fn(() => {
      throw new Error('root constraint rejected');
    });
    expect(validate({ wasmModule: { validate_json_from_str: broken } }).ok).toBe(false);
    expect(broken).toHaveBeenCalledTimes(1);
  });
});
