import { describe, expect, it } from 'vitest';
import { generateSample, listRootCandidates } from '../src/sample-gen.js';
import { validateInstance } from '../src/instance-validation.js';
import { wasmModule } from './wasm.js';

const schemas = [
  'root = { name: tstr, age: 0..130, ? active: bool }',
  'root = [uint, tstr, bool, nil]',
  'root = [2*4 uint]',
  'root = "yes" / "no"',
  'root = tstr .size 3',
  'root = uint .ge 5',
  'root = uint .gt 5',
  'root = uint .lt 10',
  'root = uint .le 10',
  'root = uint .eq 7',
  'root = uint .ne 0',
  'root = 1...5',
  'root = -10..-1',
  'root = { header, payload: [* uint] }\nheader = (version: 1, kind: tstr)',
  'root = pair<uint>\npair<T> = [T, T]',
  'root = &colors\ncolors = (red: 1, green: 2, blue: 3)',
  'root = [* root]',
  'root = { ? next: root }',
  'root = { "escaped-key": "héllo", flag: false }',
  'root = bool / uint',
];

describe('sample generation checked by Rust', () => {
  it.each(schemas)('generates a conforming instance for %s', (source) => {
    const result = generateSample(source, 'root', {
      validate(json) {
        const validation = validateInstance({ wasmModule, cddl: source, kind: 'json', text: json });
        return validation.ok ? null : validation.detail;
      },
    });
    expect(result.error).toBeNull();
    expect(result.verified, result.validationError || result.warnings.join('\n')).toBe(true);
    expect(result.json).not.toBeNull();
  });

  it.each(['bstr', "h'0102'", '#6.0(tstr)', 'undefined'])(
    'does not claim JSON fidelity for %s',
    (type) => {
      const result = generateSample(`root = ${type}`);
      expect(result.verified).toBe(false);
      expect(result.jsonRepresentable).toBe(false);
      expect(result.warnings.length).toBeGreaterThan(0);
    },
  );
  it('reports cycles, oversized occurrences and malformed schemas honestly', () => {
    expect(generateSample('root = root').constraintsSatisfied).toBe(false);
    expect(generateSample('root = [1000000*1000000 uint]').constraintsSatisfied).toBe(false);
    expect(generateSample('').error).toContain('No rules');
    expect(generateSample(`root = ${'['.repeat(100)}uint${']'.repeat(100)}`).error).not.toBeNull();
    expect(generateSample('root = uint', 'missing').warnings.join(' ')).toContain('Unknown root');
  });
  it('never treats an unverified best effort as verified', () => {
    const result = generateSample('root = tstr .regexp "impossible"', 'root', {
      validate: () => 'rejected',
    });
    expect(result.verified).toBe(false);
    expect(result.constraintsSatisfied).toBe(false);
    expect(result.validationError).toBe('rejected');
    expect(generateSample('root = uint').verified).toBe(false);
  });
  it('ranks the first concrete rule ahead of unreferenced helper rules', () => {
    expect(listRootCandidates('root = helper\nhelper = uint\nother = tstr')[0]).toBe('root');
    expect(listRootCandidates('')).toEqual([]);
    expect(listRootCandidates('generic<T> = [T]\nroot = generic<uint>')[0]).toBe('root');
  });
  it('cannot pollute Object.prototype through map keys', () => {
    const result = generateSample(
      'root = { "__proto__": { polluted: true }, "constructor": tstr }',
    );
    expect({}.polluted).toBeUndefined();
    expect(JSON.parse(result.json)).toHaveProperty('__proto__');
  });
});
