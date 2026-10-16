import { describe, expect, it } from 'vitest';
import { buildSuggestions, completionContext } from '../src/cddl-completion.js';
import { parseRules } from '../src/outline.js';

describe('rule scanning and completion', () => {
  it.each([
    ['root = ', 'type'],
    ['root = uint .si', 'control'],
    [' .', 'control'],
    ['root = my.rule', 'type'],
    ['root = "unfinished', 'none'],
    ["root = h'00", 'none'],
    ['root = uint ; comment', 'none'],
    ['root = "semi;colon" / ', 'type'],
    ['root = "escaped \\" quote" / ', 'type'],
  ])('classifies %s as %s', (prefix, expected) => {
    expect(completionContext(prefix)).toBe(expected);
  });

  it('ignores fake rules in comments, nested maps and multiline literals', () => {
    const source = `; ignored = int
root = {
  name: "fake = text",
  nested: [uint, tstr]
}
literal = h'00
not-a-rule = 11
22'
generic<T> = [T]
root /= bool
uint = 5
`;
    expect(parseRules(source).map((rule) => rule.name)).toEqual([
      'root',
      'literal',
      'generic',
      'root',
      'uint',
    ]);
    const suggestions = buildSuggestions(source, 'item = ', 'root');
    expect(suggestions.some((item) => item.label === 'root')).toBe(false);
    expect(suggestions.filter((item) => item.label === 'uint')).toHaveLength(1);
    expect(suggestions.find((item) => item.label === 'generic')?.detail).toContain('<T>');
    expect(suggestions.find((item) => item.label === 'literal')?.group).toBe('rule');
  });

  it('offers operators only after a dot and no suggestions in comments', () => {
    const operators = buildSuggestions('root = uint', 'root = uint .');
    expect(operators.length).toBeGreaterThan(30);
    expect(operators.every((item) => item.group === 'control')).toBe(true);
    expect(operators.find((item) => item.label === '.size')?.documentation).toContain('Example:');
    expect(buildSuggestions('root = uint', '; comment')).toEqual([]);
  });

  it('retains source positions across CRLF, quoted keys and augmented definitions', () => {
    const rules = parseRules('a = { "x": [uint] }\r\nb = (value: a)\r\nb //= (other: tstr)');
    expect(rules.map(({ name, line }) => ({ name, line }))).toEqual([
      { name: 'a', line: 1 },
      { name: 'b', line: 2 },
      { name: 'b', line: 3 },
    ]);
    expect(buildSuggestions(`long = "${'a'.repeat(130)}"`, 'new = ')[0].documentation).toHaveLength(
      118,
    );
    expect(parseRules('')).toEqual([]);
  });
});
