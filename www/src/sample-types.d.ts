export type JsonValue = null | boolean | number | string | JsonValue[] | JsonObject;
export interface JsonObject {
  [key: string]: JsonValue;
}
export interface Token {
  type: 'string' | 'bytes' | 'number' | 'id' | 'control' | 'op';
  value: string;
  raw: string;
}
export interface Occurrence {
  min: number;
  max: number;
  count: number;
  optional: boolean;
  raw: string;
}
export type Key =
  | { kind: 'literal'; value: JsonValue; raw: string }
  | { kind: 'name'; name: string }
  | { kind: 'computed'; node: AstNode; text: string };
export type GroupEntry =
  | { kind: 'groupChoice'; options: GroupEntry[][] }
  | { kind: 'group'; occurrence: Occurrence; entries: GroupEntry[] }
  | { kind: 'member'; occurrence: Occurrence; key: Key; value: AstNode; arrow: boolean }
  | { kind: 'element'; occurrence: Occurrence; value: AstNode };
export interface ReferenceNode {
  kind: 'ref';
  name: string;
  args: AstNode[];
}
export interface GroupNode {
  kind: 'group';
  entries: GroupEntry[];
}
export interface ControlNode {
  kind: 'control';
  op: string;
  base: AstNode;
  controller: AstNode;
}
export type AstNode =
  | { kind: 'literal'; value: JsonValue; raw: string }
  | { kind: 'bytesLiteral'; raw: string }
  | ReferenceNode
  | { kind: 'choice' | 'groupMerge'; options: AstNode[]; label: string }
  | { kind: 'range'; inclusive: boolean; lower: AstNode; upper: AstNode }
  | ControlNode
  | { kind: 'unwrap' | 'enumRef'; ref: ReferenceNode }
  | { kind: 'tag'; tag: string; inner: AstNode }
  | { kind: 'map' | 'array' | 'enum'; entries: GroupEntry[] }
  | GroupNode
  | { kind: 'unknown'; name: string };
interface Rule {
  name: string;
  params: string[];
  node: AstNode;
  choices: AstNode[];
}
export interface Model {
  rules: Map<string, Rule>;
  order: string[];
}
export type Environment = Map<string, AstNode>;
export interface Context {
  model: Model;
  warnings: string[];
  budget: number;
  jsonRepresentable: boolean;
  nonRepReasons: Set<string>;
  constraintsSatisfied: boolean;
  unappliedControls: Set<string>;
  includeOptional: boolean;
}
