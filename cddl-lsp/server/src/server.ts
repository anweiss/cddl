import {
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult,
  Hover,
  HoverParams,
  MarkupContent,
  DefinitionParams,
  DocumentFormattingParams,
  Position,
  TextEdit,
  Range,
  Location,
  SignatureHelp,
  SignatureInformation,
  ParameterInformation,
  DocumentSymbol,
  SymbolKind,
  DocumentSymbolParams,
  ReferenceParams,
  SignatureHelpParams,
  DocumentRangeFormattingParams,
  CodeActionParams,
  CodeAction,
  CodeActionKind,
  WorkspaceSymbolParams,
  SymbolInformation,
  Command,
} from "vscode-languageserver";

import { createConnection } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import * as wasm from "../pkg/";
import { standardPrelude, controlOperators } from "./keywords";
// import { WorkDoneProgress } from 'vscode-languageserver/lib/progress';

// Create a connection for the server. The connection uses Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager. The text document manager
// supports full document sync only
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let formattingEnabled: boolean = true;
// let hasDiagnosticRelatedInformationCapability: boolean = false;

let cddl: any;

connection.onInitialize((params: InitializeParams) => {
  let capabilities = params.capabilities;

  // Does the client support the `workspace/configuration` request?
  // If not, we will fall back using global settings
  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );
  // hasDiagnosticRelatedInformationCapability = !!(
  // 	capabilities.textDocument &&
  // 	capabilities.textDocument.publishDiagnostics &&
  // 	capabilities.textDocument.publishDiagnostics.relatedInformation
  // );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      // Tell the client that the server supports code completion
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: [".", "$", "=", ":", "/"],
      },
      hoverProvider: true,
      definitionProvider: true,
      referencesProvider: true,
      documentSymbolProvider: true,
      signatureHelpProvider: {
        triggerCharacters: ["<", ","],
      },
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      codeActionProvider: true,
      workspaceSymbolProvider: true,
    },
  };
  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
      },
    };
  }
  return result;
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    // Register for all configuration changes.
    connection.client.register(
      DidChangeConfigurationNotification.type,
      undefined
    );
  }
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log("Workspace folder change event received.");
    });
  }
});

// The example settings
interface ExampleSettings {
  maxNumberOfProblems: number;
  formatting: {
    enabled: boolean;
  };
  diagnostics: {
    trailingCommas: boolean;
    controlOperators: boolean;
  };
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ExampleSettings = {
  maxNumberOfProblems: 1000,
  formatting: {
    enabled: true,
  },
  diagnostics: {
    trailingCommas: true,
    controlOperators: true,
  },
};
let globalSettings: ExampleSettings = defaultSettings;

// Cache the settings of all open documents
let documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onDidChangeConfiguration((change) => {
  if (hasConfigurationCapability) {
    // Reset all cached document settings
    documentSettings.clear();
  } else {
    globalSettings = <ExampleSettings>(change.settings.cddl || defaultSettings);
  }

  // Revalidate all open text documents
  documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings);
  }
  let result = documentSettings.get(resource);
  if (!result) {
    result = connection.workspace.getConfiguration({
      scopeUri: resource,
      section: "cddl",
    });
    documentSettings.set(resource, result);
  }
  return result;
}

// Only keep settings for open documents
documents.onDidClose((e) => {
  documentSettings.delete(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent((change) => {
  validateTextDocument(change.document);
});

// Enhanced validation for semantic errors and warnings
function performEnhancedValidation(
  textDocument: TextDocument,
  cddl: any,
  settings: ExampleSettings
): Diagnostic[] {
  let diagnostics: Diagnostic[] = [];
  let text = textDocument.getText();
  let lines = text.split("\n");

  // Track defined and used rules
  let definedRules: Set<string> = new Set();
  let usedRules: Set<string> = new Set();
  let ruleDefinitions: Map<
    string,
    { line: number; range: Range; nameRange: Range }
  > = new Map();

  // Add control operator validation
  if (settings.diagnostics?.controlOperators) {
    diagnostics.push(...validateControlOperators(textDocument, cddl, lines));

    // Add pattern-based control operator validation (fallback for cases AST doesn't catch)
    diagnostics.push(...validateControlOperatorPatterns(textDocument, lines));
  }

  // Collect all defined rules
  for (let rule of cddl.rules) {
    if (rule.Group) {
      let ruleName = rule.Group.rule.name.ident;
      definedRules.add(ruleName);
      let range = findRuleRange(ruleName, lines);
      let nameRange = findRuleNameRange(ruleName, lines);
      ruleDefinitions.set(ruleName, {
        line: range.start.line,
        range,
        nameRange,
      });
    }
    if (rule.Type) {
      let ruleName = rule.Type.rule.name.ident;
      definedRules.add(ruleName);
      let range = findRuleRange(ruleName, lines);
      let nameRange = findRuleNameRange(ruleName, lines);
      ruleDefinitions.set(ruleName, {
        line: range.start.line,
        range,
        nameRange,
      });
    }
  }

  // Find rule references in the text
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];

    // For lines with assignments, only check the right side of the assignment
    if (line.includes("=")) {
      let assignmentMatch = line.match(/^([^=]*)(\/\/=|\/=|=)(.*)$/);
      if (assignmentMatch) {
        let rightSide = assignmentMatch[3]; // Everything after the assignment operator

        // Look for rule references on the right side of assignments
        for (let ruleName of definedRules) {
          // Use custom word boundaries for CDDL identifiers (including hyphens)
          let regex = new RegExp(
            `(?<![a-zA-Z0-9_-])${escapeRegex(ruleName)}(?![a-zA-Z0-9_-])`,
            "g"
          );
          if (regex.test(rightSide)) {
            usedRules.add(ruleName);
          }
        }
      }
    } else {
      // For lines without assignments, check the entire line
      for (let ruleName of definedRules) {
        let regex = new RegExp(
          `(?<![a-zA-Z0-9_-])${escapeRegex(ruleName)}(?![a-zA-Z0-9_-])`,
          "g"
        );
        if (regex.test(line)) {
          usedRules.add(ruleName);
        }
      }
    }

    // Check for map key references (rule-name =>)
    for (let ruleName of definedRules) {
      let mapKeyRegex = new RegExp(
        `(?<![a-zA-Z0-9_-])${escapeRegex(ruleName)}\\s*=>`,
        "g"
      );
      if (mapKeyRegex.test(line)) {
        usedRules.add(ruleName);
      }
    }

    // Also check for socket/plug references ($$rule-name)
    let socketPlugMatches = line.match(/\$\$([a-zA-Z][a-zA-Z0-9-_]*)/g);
    if (socketPlugMatches) {
      for (let match of socketPlugMatches) {
        let plugName = match.substring(2); // Remove $$
        if (definedRules.has(plugName)) {
          usedRules.add(plugName);
        }
      }
    }

    // Check for $rule references (choice sockets)
    let choiceSocketMatches = line.match(/\$([a-zA-Z][a-zA-Z0-9-_]*)/g);
    if (choiceSocketMatches) {
      for (let match of choiceSocketMatches) {
        let choiceName = match.substring(1); // Remove $
        if (definedRules.has(choiceName)) {
          usedRules.add(choiceName);
        }
      }
    }
  }

  // Report unused rules
  for (let ruleName of definedRules) {
    if (!usedRules.has(ruleName)) {
      let ruleInfo = ruleDefinitions.get(ruleName);
      if (ruleInfo) {
        // Skip warning for certain patterns that are typically used as external references
        // - Integer constant definitions (rule = number)
        // - String constant definitions (rule = "string")
        // - Root/entry point rules (common naming patterns)
        let ruleLineText = lines[ruleInfo.line] || "";

        // Check if this is an integer constant definition
        let isIntegerConstant = /=\s*\d+\s*$/.test(ruleLineText.trim());

        // Check if this is a string constant definition
        let isStringConstant = /=\s*"[^"]*"\s*$/.test(ruleLineText.trim());

        // Check if this is likely a root rule (entry point)
        let isLikelyRootRule =
          ruleName.includes("tag") ||
          ruleName.includes("entry") ||
          ruleName.includes("root") ||
          ruleName.includes("main");

        if (!isIntegerConstant && !isStringConstant && !isLikelyRootRule) {
          diagnostics.push({
            severity: DiagnosticSeverity.Warning,
            range: ruleInfo.nameRange, // Use nameRange instead of range for unused rule warnings
            message: `Rule '${ruleName}' is defined but never used`,
            source: "cddl-lint",
            code: "unused-rule",
          });
        }
      }
    }
  }

  // Check for potential circular dependencies (simple check)
  let circularDeps = findCircularDependencies(cddl);
  for (let dep of circularDeps) {
    let ruleInfo = ruleDefinitions.get(dep);
    if (ruleInfo) {
      diagnostics.push({
        severity: DiagnosticSeverity.Error,
        range: ruleInfo.range,
        message: `Potential circular dependency detected in rule '${dep}'`,
        source: "cddl-lint",
        code: "circular-dependency",
      });
    }
  }

  // Check for common style issues
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];

    // Check for missing spaces around operators
    if (/\w[=\/]{1,2}=\w/.test(line)) {
      let match = line.match(/\w[=\/]{1,2}=\w/);
      if (match) {
        let startChar = line.indexOf(match[0]);
        diagnostics.push({
          severity: DiagnosticSeverity.Information,
          range: {
            start: { line: i, character: startChar },
            end: { line: i, character: startChar + match[0].length },
          },
          message:
            "Consider adding spaces around assignment operators for better readability",
          source: "cddl-style",
          code: "spacing-around-operators",
        });
      }
    }

    // Check for trailing commas in single-line definitions
    if (
      settings.diagnostics?.trailingCommas &&
      line.trim().endsWith(",") &&
      !line.includes("{") &&
      !line.includes("[")
    ) {
      diagnostics.push({
        severity: DiagnosticSeverity.Information,
        range: {
          start: { line: i, character: line.length - 1 },
          end: { line: i, character: line.length },
        },
        message: "Trailing comma may not be necessary",
        source: "cddl-style",
        code: "trailing-comma",
      });
    }
  }

  return diagnostics;
}

// Helper function to get valid types for a control operator
function getValidTypesForControlOperator(controlOp: string): string {
  const validTypes: { [key: string]: string[] } = {
    ".size": ["text", "tstr", "bytes", "bstr", "array", "map"],
    ".bits": ["uint", "bytes", "bstr"],
    ".regexp": ["text", "tstr"],
    ".pcre": ["text", "tstr"],
    ".cbor": ["bytes", "bstr"],
    ".cborseq": ["bytes", "bstr"],
    ".within": ["any"],
    ".and": ["any"],
    ".lt": ["uint", "int", "float", "number"],
    ".le": ["uint", "int", "float", "number"],
    ".gt": ["uint", "int", "float", "number"],
    ".ge": ["uint", "int", "float", "number"],
    ".eq": ["uint", "int", "float", "number"],
    ".ne": ["uint", "int", "float", "number"],
  };

  const types = validTypes[controlOp];
  if (!types || types.length === 0) {
    return "no types";
  } else if (types.includes("any")) {
    return "any type";
  } else if (types.length === 1) {
    return types[0];
  } else if (types.length === 2) {
    return `${types[0]} or ${types[1]}`;
  } else {
    return `${types.slice(0, -1).join(", ")}, or ${types[types.length - 1]}`;
  }
}

// Validate control operator usage for semantic errors
function validateControlOperators(
  textDocument: TextDocument,
  cddl: any,
  lines: string[]
): Diagnostic[] {
  let diagnostics: Diagnostic[] = [];

  // Pattern-based validation for common invalid control operator combinations
  for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    let line = lines[lineIndex];

    // Check for invalid control operators on uint/int with .regexp/.pcre
    if (/\b(uint|int)\s+\.(?:regexp|pcre)/.test(line)) {
      let match = line.match(/\b(uint|int)\s+(\.(?:regexp|pcre))/);
      if (match) {
        let startChar = match.index! + match[1].length + 1;
        let endChar = startChar + match[2].length;
        let validTypes = getValidTypesForControlOperator(match[2]);
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: lineIndex, character: startChar },
            end: { line: lineIndex, character: endChar },
          },
          message: `Control operator ${match[2]} cannot be applied to ${match[1]} type. Valid types: ${validTypes}`,
          source: "cddl-control",
          code: "invalid-control-operator",
        });
      }
    }

    // Check for invalid .bits on text strings
    if (/\b(text|tstr)\s+\.bits/.test(line)) {
      let match = line.match(/\b(text|tstr)\s+(\.bits)/);
      if (match) {
        let startChar = match.index! + match[1].length + 1;
        let endChar = startChar + match[2].length;
        let validTypes = getValidTypesForControlOperator(match[2]);
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: lineIndex, character: startChar },
            end: { line: lineIndex, character: endChar },
          },
          message: `Control operator ${match[2]} cannot be applied to ${match[1]} type. Valid types: ${validTypes}`,
          source: "cddl-control",
          code: "invalid-control-operator",
        });
      }
    }

    // Check for invalid .cbor on text strings
    if (/\b(text|tstr)\s+\.cbor/.test(line)) {
      let match = line.match(/\b(text|tstr)\s+(\.cbor)/);
      if (match) {
        let startChar = match.index! + match[1].length + 1;
        let endChar = startChar + match[2].length;
        let validTypes = getValidTypesForControlOperator(match[2]);
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: lineIndex, character: startChar },
            end: { line: lineIndex, character: endChar },
          },
          message: `Control operator ${match[2]} cannot be applied to ${match[1]} type. Valid types: ${validTypes}`,
          source: "cddl-control",
          code: "invalid-control-operator",
        });
      }
    }

    // Check for invalid .size on bool
    if (/\b(bool|true|false)\s+\.size/.test(line)) {
      let match = line.match(/\b(bool|true|false)\s+(\.size)/);
      if (match) {
        let startChar = match.index! + match[1].length + 1;
        let endChar = startChar + match[2].length;
        let validTypes = getValidTypesForControlOperator(match[2]);
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: lineIndex, character: startChar },
            end: { line: lineIndex, character: endChar },
          },
          message: `Control operator ${match[2]} cannot be applied to ${match[1]} type. Valid types: ${validTypes}`,
          source: "cddl-control",
          code: "invalid-control-operator",
        });
      }
    }
  }

  return diagnostics;
}

function validateTypeRuleControlOperators(
  typeRule: any,
  textDocument: TextDocument,
  diagnostics: Diagnostic[],
  lines: string[]
): void {
  try {
    traverseType(typeRule.value, textDocument, diagnostics, lines);
  } catch (e) {
    // Skip if we can't traverse the type
  }
}

function validateGroupRuleControlOperators(
  groupRule: any,
  textDocument: TextDocument,
  diagnostics: Diagnostic[],
  lines: string[]
): void {
  try {
    if (groupRule.entry && Array.isArray(groupRule.entry)) {
      for (let entry of groupRule.entry) {
        if (
          entry.GroupEntry &&
          entry.GroupEntry.member_key &&
          entry.GroupEntry.member_key.type1
        ) {
          traverseType(
            entry.GroupEntry.member_key.type1,
            textDocument,
            diagnostics,
            lines
          );
        }
        if (entry.GroupEntry && entry.GroupEntry.entry_type) {
          traverseType(
            entry.GroupEntry.entry_type,
            textDocument,
            diagnostics,
            lines
          );
        }
      }
    }
  } catch (e) {
    // Skip if we can't traverse the group
  }
}

function traverseType(
  type: any,
  textDocument: TextDocument,
  diagnostics: Diagnostic[],
  lines: string[]
): void {
  if (!type) return;

  try {
    // Handle Type1 (choice alternatives)
    if (type.type2 && Array.isArray(type.type2)) {
      for (let type2 of type.type2) {
        traverseType2(type2, textDocument, diagnostics, lines);
      }
    } else if (type.type2) {
      traverseType2(type.type2, textDocument, diagnostics, lines);
    }
  } catch (e) {
    // Skip malformed types
  }
}

function traverseType2(
  type2: any,
  textDocument: TextDocument,
  diagnostics: Diagnostic[],
  lines: string[]
): void {
  if (!type2) return;

  try {
    // Check for control operators
    if (type2.operator && type2.controller) {
      validateControlOperatorUsage(
        type2,
        type2.operator,
        type2.controller,
        textDocument,
        diagnostics,
        lines
      );
    }

    // Recursively check nested types
    if (type2.value) {
      traverseValue(type2.value, textDocument, diagnostics, lines);
    }
  } catch (e) {
    // Skip malformed type2
  }
}

function traverseValue(
  value: any,
  textDocument: TextDocument,
  diagnostics: Diagnostic[],
  lines: string[]
): void {
  if (!value) return;

  try {
    // Handle arrays
    if (value.Array && value.Array.group && value.Array.group.entry) {
      if (Array.isArray(value.Array.group.entry)) {
        for (let entry of value.Array.group.entry) {
          if (entry.GroupEntry && entry.GroupEntry.entry_type) {
            traverseType(
              entry.GroupEntry.entry_type,
              textDocument,
              diagnostics,
              lines
            );
          }
        }
      }
    }

    // Handle maps
    if (value.Map && value.Map.group && value.Map.group.entry) {
      if (Array.isArray(value.Map.group.entry)) {
        for (let entry of value.Map.group.entry) {
          if (entry.GroupEntry) {
            if (
              entry.GroupEntry.member_key &&
              entry.GroupEntry.member_key.type1
            ) {
              traverseType(
                entry.GroupEntry.member_key.type1,
                textDocument,
                diagnostics,
                lines
              );
            }
            if (entry.GroupEntry.entry_type) {
              traverseType(
                entry.GroupEntry.entry_type,
                textDocument,
                diagnostics,
                lines
              );
            }
          }
        }
      }
    }

    // Handle groups
    if (value.Group && value.Group.group && value.Group.group.entry) {
      if (Array.isArray(value.Group.group.entry)) {
        for (let entry of value.Group.group.entry) {
          if (entry.GroupEntry && entry.GroupEntry.entry_type) {
            traverseType(
              entry.GroupEntry.entry_type,
              textDocument,
              diagnostics,
              lines
            );
          }
        }
      }
    }
  } catch (e) {
    // Skip malformed values
  }
}

function validateControlOperatorUsage(
  type2: any,
  operator: any,
  controller: any,
  textDocument: TextDocument,
  diagnostics: Diagnostic[],
  lines: string[]
): void {
  if (!operator || !operator.span || operator.span.length < 2) {
    return;
  }

  try {
    let startPos = textDocument.positionAt(operator.span[0]);
    let endPos = textDocument.positionAt(operator.span[1]);

    let targetType = getTargetTypeName(type2);
    let controlOperatorName = getControlOperatorName(operator);

    if (!controlOperatorName || !targetType) {
      return;
    }

    let errorMessage = getControlOperatorError(
      controlOperatorName,
      targetType,
      controller
    );

    if (errorMessage) {
      diagnostics.push({
        severity: DiagnosticSeverity.Error,
        range: {
          start: startPos,
          end: endPos,
        },
        message: errorMessage,
        source: "cddl-control-operator",
        code: "invalid-control-operator",
      });
    }
  } catch (e) {
    // Skip if we can't create the diagnostic
  }
}

function getTargetTypeName(type2: any): string | null {
  if (!type2 || !type2.value) {
    return null;
  }

  try {
    // Check for direct typename
    if (type2.value.Typename && type2.value.Typename.ident) {
      return type2.value.Typename.ident;
    }

    // Check for value types
    if (type2.value.Value) {
      if (type2.value.Value.TEXT) return "tstr";
      if (type2.value.Value.UINT !== undefined) return "uint";
      if (type2.value.Value.NINT !== undefined) return "nint";
      if (type2.value.Value.INT !== undefined) return "int";
      if (type2.value.Value.FLOAT !== undefined) return "float";
      if (type2.value.Value.BYTE) return "bstr";
    }

    // Check for literal values
    if (type2.value.Array) return "array";
    if (type2.value.Map) return "map";
    if (type2.value.Group) return "group";

    return null;
  } catch (e) {
    return null;
  }
}

function getControlOperatorName(operator: any): string | null {
  try {
    if (operator.SIZE !== undefined) return ".size";
    if (operator.BITS !== undefined) return ".bits";
    if (operator.REGEXP !== undefined) return ".regexp";
    if (operator.PCRE !== undefined) return ".pcre";
    if (operator.CBOR !== undefined) return ".cbor";
    if (operator.CBORSEQ !== undefined) return ".cborseq";
    if (operator.WITHIN !== undefined) return ".within";
    if (operator.AND !== undefined) return ".and";
    if (operator.LT !== undefined) return ".lt";
    if (operator.LE !== undefined) return ".le";
    if (operator.GT !== undefined) return ".gt";
    if (operator.GE !== undefined) return ".ge";
    if (operator.EQ !== undefined) return ".eq";
    if (operator.NE !== undefined) return ".ne";
    if (operator.DEFAULT !== undefined) return ".default";

    return null;
  } catch (e) {
    return null;
  }
}

function getControlOperatorError(
  controlOp: string,
  targetType: string,
  controller: any
): string | null {
  // Define type compatibility rules based on CDDL spec and implementation
  const stringTypes = new Set(["tstr", "text", "bstr", "bytes"]);
  const numericTypes = new Set([
    "uint",
    "nint",
    "int",
    "float",
    "float16",
    "float32",
    "float64",
    "number",
    "time",
  ]);
  const uintTypes = new Set(["uint"]);
  const textStringTypes = new Set(["tstr", "text"]);
  const byteStringTypes = new Set(["bstr", "bytes"]);
  const booleanTypes = new Set(["bool", "true", "false"]);
  const nullTypes = new Set(["nil", "null", "undefined"]);

  switch (controlOp) {
    case ".size":
      if (
        !stringTypes.has(targetType) &&
        !uintTypes.has(targetType) &&
        targetType !== "array" &&
        targetType !== "map"
      ) {
        return `Control operator .size can only be applied to string, uint, array, or map types, but got ${targetType}`;
      }
      break;

    case ".bits":
      if (!byteStringTypes.has(targetType) && !uintTypes.has(targetType)) {
        return `Control operator .bits can only be applied to byte string or uint types, but got ${targetType}`;
      }
      break;

    case ".regexp":
    case ".pcre":
      if (!textStringTypes.has(targetType)) {
        return `Control operator ${controlOp} can only be applied to text string types, but got ${targetType}`;
      }
      break;

    case ".cbor":
    case ".cborseq":
      if (!byteStringTypes.has(targetType)) {
        return `Control operator ${controlOp} can only be applied to byte string types, but got ${targetType}`;
      }
      break;

    case ".lt":
    case ".le":
    case ".gt":
    case ".ge":
      if (!numericTypes.has(targetType)) {
        return `Control operator ${controlOp} can only be applied to numeric types, but got ${targetType}`;
      }
      break;

    case ".eq":
    case ".ne":
      // These can be applied to any type
      break;

    case ".and":
    case ".within":
      // These have more complex validation rules that would require deeper analysis
      break;

    case ".default":
      // Should only be used in optional map members - this would require context analysis
      break;

    default:
      return null;
  }

  return null;
}

// Add pattern-based validation for control operators directly from text
function validateControlOperatorPatterns(
  textDocument: TextDocument,
  lines: string[]
): Diagnostic[] {
  let diagnostics: Diagnostic[] = [];

  for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    let line = lines[lineIndex];

    // Pattern to match type followed by control operator
    let patterns = [
      // uint/int/numeric types with string control operators
      {
        regex:
          /(uint|nint|int|float(?:16|32|64)?|number|time)\s*(\\.(?:regexp|pcre|cbor|cborseq))/g,
        message: (match: RegExpMatchArray) => {
          const validTypes = getValidTypesForControlOperator(match[2]);
          return `Control operator ${match[2]} cannot be applied to numeric type ${match[1]}. Valid types: ${validTypes}`;
        },
      },
      // text string types with non-text control operators
      {
        regex: /(tstr|text)\s*(\\.(?:bits|cbor|cborseq))/g,
        message: (match: RegExpMatchArray) => {
          const validTypes = getValidTypesForControlOperator(match[2]);
          return `Control operator ${match[2]} cannot be applied to text string type ${match[1]}. Valid types: ${validTypes}`;
        },
      },
      // byte string types with text control operators
      {
        regex: /(bstr|bytes)\s*(\\.(?:regexp|pcre))/g,
        message: (match: RegExpMatchArray) => {
          const validTypes = getValidTypesForControlOperator(match[2]);
          return `Control operator ${match[2]} cannot be applied to byte string type ${match[1]}. Valid types: ${validTypes}`;
        },
      },
      // boolean and null types with inappropriate control operators
      {
        regex:
          /(bool|true|false|nil|null|undefined)\s*(\\.(?:size|bits|regexp|pcre|cbor|cborseq|lt|le|gt|ge))/g,
        message: (match: RegExpMatchArray) => {
          const validTypes = getValidTypesForControlOperator(match[2]);
          return `Control operator ${match[2]} cannot be applied to ${match[1]} type. Valid types: ${validTypes}`;
        },
      },
    ];

    for (let pattern of patterns) {
      let match;
      while ((match = pattern.regex.exec(line)) !== null) {
        let startChar = match.index + match[1].length;
        let endChar = match.index + match[0].length;

        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: lineIndex, character: startChar },
            end: { line: lineIndex, character: endChar },
          },
          message: pattern.message(match),
          source: "cddl-syntax",
          code: "invalid-control-operator-syntax",
        });
      }
    }
  }

  return diagnostics;
}

function findCircularDependencies(cddl: any): string[] {
  let dependencies: Map<string, Set<string>> = new Map();
  let circular: string[] = [];

  // Build dependency graph
  for (let rule of cddl.rules) {
    let ruleName = "";
    if (rule.Group) ruleName = rule.Group.rule.name.ident;
    if (rule.Type) ruleName = rule.Type.rule.name.ident;

    if (ruleName) {
      dependencies.set(ruleName, new Set());
      // This is a simplified check - would need proper AST traversal for full accuracy
    }
  }

  // Simple circular dependency detection (would need enhancement for real use)
  // For now, just return empty array as this needs deeper AST analysis
  return circular;
}

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  // In this simple example we get the settings for every validate run.
  let settings = await getDocumentSettings(textDocument.uri);

  // The validator creates diagnostics for all uppercase words length 2 and more
  let text = textDocument.getText();

  let errors: any[] = [];
  try {
    cddl = wasm.cddl_from_str(text);
  } catch (e) {
    errors = Array.isArray(e) ? e : [e];
  }

  let diagnostics: Diagnostic[] = [];

  // Add syntax/parse errors
  for (const error of errors) {
    if (diagnostics.length >= settings.maxNumberOfProblems) {
      break;
    }

    // Check if error has position information
    if (
      !error.position ||
      !error.position.range ||
      !Array.isArray(error.position.range) ||
      error.position.range.length < 2
    ) {
      // If no position info, create a diagnostic for the whole first line
      let diagnostic: Diagnostic = {
        severity: DiagnosticSeverity.Error,
        range: {
          start: { line: 0, character: 0 },
          end: { line: 0, character: Number.MAX_VALUE },
        },
        message: error.msg?.short || error.message || "Parse error",
        source: "cddl",
      };
      diagnostics.push(diagnostic);
      continue;
    }

    let diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Error,
      range: {
        start: textDocument.positionAt(error.position.range[0]),
        end: textDocument.positionAt(error.position.range[1]),
      },
      message: error.msg.short,
      source: "cddl",
    };

    diagnostics.push(diagnostic);
  }

  // Enhanced validation - add semantic errors and warnings
  if (cddl) {
    diagnostics.push(
      ...performEnhancedValidation(textDocument, cddl, settings)
    );
  }

  // Send the computed diagnostics to VSCode.
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

connection.onDidChangeWatchedFiles((_change) => {
  // Monitored files have change in VSCode
  connection.console.log("We received an file change event");
});

let triggeredOnControl = false;
let triggeredOnPlug = false;

// This handler provides the initial list of the completion items.
connection.onCompletion(
  (textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
    // Reset trigger flags for clean state
    triggeredOnControl = false;
    triggeredOnPlug = false;

    let completionItems: CompletionItem[] = [];
    let document = documents.get(textDocumentPosition.textDocument.uri);

    if (!document) {
      return completionItems;
    }

    let offset = document.offsetAt(textDocumentPosition.position);
    let text = document.getText();
    let line = document.getText({
      start: { line: textDocumentPosition.position.line, character: 0 },
      end: textDocumentPosition.position,
    });

    // Get context around current position
    let beforeCursor = text.substring(Math.max(0, offset - 10), offset);
    let afterCursor = text.substring(
      offset,
      Math.min(text.length, offset + 10)
    );

    // Check if we're in a rule assignment context
    let isInRuleAssignment = /\w+\s*(<[^>]*>)?\s*$/.test(line);

    // Check if we're after an assignment operator
    let isAfterAssignment = /(=|\/=|\/\/=)\s*$/.test(line);

    // Check if we're in a type context (after :)
    let isInTypeContext = /:\s*$/.test(line);

    // If character is leading '.', then only emit controls
    if (beforeCursor.endsWith(".")) {
      triggeredOnControl = true;

      for (let index = 0; index < controlOperators.length; index++) {
        completionItems.push({
          label: controlOperators[index].label,
          kind: CompletionItemKind.Operator,
          data: index,
          documentation: controlOperators[index].documentation,
          detail: controlOperators[index].detail,
          insertText: controlOperators[index].label.substring(1), // Remove the '.'
        });
      }

      return completionItems;
    }

    // Socket/plug completions
    if (beforeCursor.endsWith("$$") || beforeCursor.endsWith("$")) {
      triggeredOnPlug = true;

      if (cddl) {
        let groupPlugs: Set<string> = new Set();
        let typePlugs: Set<string> = new Set();

        for (let rule of cddl.rules) {
          if (rule.Group && rule.Group.rule.name.socket === "GROUP") {
            groupPlugs.add(rule.Group.rule.name.ident);
          }
          if (rule.Type && rule.Type.rule.name.socket === "TYPE") {
            typePlugs.add(rule.Type.rule.name.ident);
          }
        }

        if (beforeCursor.endsWith("$$")) {
          for (let groupPlug of groupPlugs) {
            completionItems.push({
              label: "$$" + groupPlug,
              kind: CompletionItemKind.Variable,
              insertText: "$$" + groupPlug,
              detail: "Group socket plug",
            });
          }
        } else if (beforeCursor.endsWith("$")) {
          for (let typePlug of typePlugs) {
            completionItems.push({
              label: "$" + typePlug,
              kind: CompletionItemKind.Variable,
              insertText: "$" + typePlug,
              detail: "Type socket plug",
            });
          }
        }

        return completionItems;
      }
    }

    // Add assignment operators when in rule context
    if (isInRuleAssignment) {
      completionItems.push(
        {
          label: "=",
          kind: CompletionItemKind.Operator,
          detail: "Type assignment",
        },
        {
          label: "//=",
          kind: CompletionItemKind.Operator,
          detail: "Group assignment",
        },
        {
          label: "/=",
          kind: CompletionItemKind.Operator,
          detail: "Choice assignment",
        }
      );
    }

    // Add standard prelude types
    for (let index = 0; index < standardPrelude.length; index++) {
      completionItems.push({
        label: standardPrelude[index].label,
        kind: CompletionItemKind.TypeParameter,
        data: index,
        documentation: standardPrelude[index].documentation,
        detail: standardPrelude[index].detail,
      });
    }

    // Add user-defined rules from current document
    if (cddl) {
      let seenRules: Set<string> = new Set();

      for (let rule of cddl.rules) {
        if (rule.Group && !seenRules.has(rule.Group.rule.name.ident)) {
          seenRules.add(rule.Group.rule.name.ident);
          completionItems.push({
            label: rule.Group.rule.name.ident,
            kind: CompletionItemKind.Class,
            detail: "Group rule",
          });
        }
        if (rule.Type && !seenRules.has(rule.Type.rule.name.ident)) {
          seenRules.add(rule.Type.rule.name.ident);
          completionItems.push({
            label: rule.Type.rule.name.ident,
            kind: CompletionItemKind.Class,
            detail: "Type rule",
          });
        }
      }
    }

    // Add operators and keywords based on context
    if (isAfterAssignment || isInTypeContext) {
      completionItems.push(
        {
          label: "{",
          kind: CompletionItemKind.Snippet,
          insertText: "{\n\t$0\n}",
          detail: "Map",
        },
        {
          label: "[",
          kind: CompletionItemKind.Snippet,
          insertText: "[\n\t$0\n]",
          detail: "Array",
        },
        {
          label: "(",
          kind: CompletionItemKind.Snippet,
          insertText: "($0)",
          detail: "Group",
        }
      );
    }

    return completionItems;
  }
);

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
  if (triggeredOnControl) {
    for (let index = 0; index < controlOperators.length; index++) {
      if (item.data === index) {
        item.insertText = item.label.substring(1);
        return item;
      }
    }
  }

  if (triggeredOnPlug) {
    // The insertText is already correctly set in the completion items
    // No need to modify it here
    return item;
  }

  for (let index = 0; index < standardPrelude.length; index++) {
    if (item.data === index) {
      item.detail = standardPrelude[index].detail;
      break;
    }
  }

  return item;
});

// Document symbol provider for outline view
connection.onDocumentSymbol(
  (params: DocumentSymbolParams): DocumentSymbol[] => {
    let document = documents.get(params.textDocument.uri);
    if (!document || !cddl) {
      return [];
    }

    let symbols: DocumentSymbol[] = [];
    let text = document.getText();
    let lines = text.split("\n");
    let seenSymbols: Set<string> = new Set();

    for (let rule of cddl.rules) {
      if (rule.Group) {
        let ruleName = rule.Group.rule.name.ident;
        if (!seenSymbols.has(ruleName)) {
          seenSymbols.add(ruleName);
          let range = findRuleRange(ruleName, lines);
          let nameRange = findRuleNameRange(ruleName, lines);

          // Ensure selectionRange is contained within range
          // If nameRange is invalid or not contained, fall back to using range
          let selectionRange = nameRange;
          if (
            nameRange.start.line < range.start.line ||
            nameRange.end.line > range.end.line ||
            (nameRange.start.line === range.start.line &&
              nameRange.start.character < range.start.character) ||
            (nameRange.end.line === range.end.line &&
              nameRange.end.character > range.end.character)
          ) {
            selectionRange = range;
          }

          symbols.push({
            name: ruleName,
            kind: SymbolKind.Class,
            range: range,
            selectionRange: selectionRange,
            detail: "Group rule",
          });
        }
      }

      if (rule.Type) {
        let ruleName = rule.Type.rule.name.ident;
        if (!seenSymbols.has(ruleName)) {
          seenSymbols.add(ruleName);
          let range = findRuleRange(ruleName, lines);
          let nameRange = findRuleNameRange(ruleName, lines);

          // Ensure selectionRange is contained within range
          // If nameRange is invalid or not contained, fall back to using range
          let selectionRange = nameRange;
          if (
            nameRange.start.line < range.start.line ||
            nameRange.end.line > range.end.line ||
            (nameRange.start.line === range.start.line &&
              nameRange.start.character < range.start.character) ||
            (nameRange.end.line === range.end.line &&
              nameRange.end.character > range.end.character)
          ) {
            selectionRange = range;
          }

          symbols.push({
            name: ruleName,
            kind: SymbolKind.TypeParameter,
            range: range,
            selectionRange: selectionRange,
            detail: "Type rule",
          });
        }
      }
    }

    return symbols;
  }
);

function findRuleRange(ruleName: string, lines: string[]): Range {
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    if (
      line.includes(ruleName) &&
      (line.includes("=") || line.includes("//=") || line.includes("/="))
    ) {
      let start = { line: i, character: 0 };
      let end = { line: i, character: line.length };

      // Extend to include multi-line rules
      let j = i + 1;
      while (
        j < lines.length &&
        (lines[j].startsWith(" ") || lines[j].startsWith("\t"))
      ) {
        j++;
      }
      if (j > i + 1) {
        end = { line: j - 1, character: lines[j - 1].length };
      }

      return { start, end };
    }
  }

  // Fallback to first line if not found
  return { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } };
}

function findRuleNameRange(ruleName: string, lines: string[]): Range {
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    if (
      line.includes(ruleName) &&
      (line.includes("=") || line.includes("//=") || line.includes("/="))
    ) {
      // Find the exact position of the rule name in the line
      let ruleIndex = line.indexOf(ruleName);
      if (ruleIndex !== -1) {
        // Make sure this is actually the rule definition, not just a reference
        let beforeRule = line.substring(0, ruleIndex).trim();
        let afterRule = line.substring(ruleIndex + ruleName.length);

        // Check if this looks like a rule definition (rule name should be at start or after whitespace)
        if (beforeRule === "" || /^\s*$/.test(beforeRule)) {
          // Check if there's an assignment operator after the rule name (possibly with generics)
          if (/^\s*(<[^>]*>)?\s*(=|\/\/=|\/=)/.test(afterRule)) {
            return {
              start: { line: i, character: ruleIndex },
              end: { line: i, character: ruleIndex + ruleName.length },
            };
          }
        }
      }
    }
  }

  // Fallback: if we can't find the rule name specifically, return the start of the rule line
  // This ensures the selection range is always contained within the full range
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    if (
      line.includes(ruleName) &&
      (line.includes("=") || line.includes("//=") || line.includes("/="))
    ) {
      // Find the first non-whitespace character (likely the start of the rule name)
      let firstChar = line.search(/\S/);
      if (firstChar !== -1) {
        return {
          start: { line: i, character: firstChar },
          end: { line: i, character: firstChar + ruleName.length },
        };
      }
    }
  }

  // Ultimate fallback: return a zero-width range at the start of the first line
  return { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } };
} // References provider for jump to definition
connection.onReferences((params: ReferenceParams): Location[] => {
  let document = documents.get(params.textDocument.uri);
  if (!document || !cddl) {
    return [];
  }

  let text = document.getText();
  let position = params.position;
  let offset = document.offsetAt(position);
  let word = getWordAtPosition(text, offset);

  if (!word) {
    return [];
  }

  let locations: Location[] = [];
  let lines = text.split("\n");

  // Find all references to the word
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    let index = 0;

    while ((index = line.indexOf(word, index)) !== -1) {
      // Make sure it's a whole word match
      let before = index > 0 ? line[index - 1] : " ";
      let after =
        index + word.length < line.length ? line[index + word.length] : " ";

      if (!/\w/.test(before) && !/\w/.test(after)) {
        locations.push({
          uri: params.textDocument.uri,
          range: {
            start: { line: i, character: index },
            end: { line: i, character: index + word.length },
          },
        });
      }

      index += word.length;
    }
  }

  return locations;
});

function getWordAtPosition(text: string, offset: number): string | null {
  let start = offset;
  let end = offset;

  // CDDL identifiers can contain letters, digits, hyphens, and underscores
  let identifierChar = /[a-zA-Z0-9_-]/;

  // Find start of word
  while (start > 0 && identifierChar.test(text[start - 1])) {
    start--;
  }

  // Find end of word
  while (end < text.length && identifierChar.test(text[end])) {
    end++;
  }

  if (start === end) {
    return null;
  }

  return text.substring(start, end);
}

function escapeRegex(string: string): string {
  return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function preserveBlankLines(text: string): string {
  // Replace sequences of blank lines with comment markers
  // This handles both single blank lines and multiple blank lines
  return text.replace(/\n(\s*\n)+/g, (match) => {
    // Count how many blank lines there are
    const blankLineCount = (match.match(/\n/g) || []).length - 1;
    // Preserve at most one blank line by using one marker
    return "\n; __BLANK_LINE_MARKER__\n";
  });
}

function restoreBlankLines(text: string): string {
  // Replace the markers back with blank lines, normalizing to single blank lines
  return text.replace(/\s*;\s*__BLANK_LINE_MARKER__\s*\n/g, "\n\n");
}

// Signature help provider for generic parameters and control operators
connection.onSignatureHelp(
  (params: SignatureHelpParams): SignatureHelp | null => {
    let document = documents.get(params.textDocument.uri);
    if (!document) {
      return null;
    }

    let text = document.getText();
    let position = params.position;
    let line = document.getText({
      start: { line: position.line, character: 0 },
      end: position,
    });

    // Check if we're in a generic context (between < and >)
    let genericMatch = line.match(/(\w+)\s*<[^>]*$/);
    if (genericMatch) {
      return {
        signatures: [
          {
            label: `${genericMatch[1]}<parameter>`,
            documentation: "Generic type parameter",
            parameters: [
              {
                label: "parameter",
                documentation: "Type parameter for generic",
              },
            ],
          },
        ],
        activeSignature: 0,
        activeParameter: 0,
      };
    }

    // Check for control operators
    let controlMatch = line.match(/\.(\w*)$/);
    if (controlMatch) {
      let partial = controlMatch[1];
      let matchingControls = controlOperators.filter((op) =>
        op.label.substring(1).startsWith(partial)
      );

      if (matchingControls.length > 0) {
        return {
          signatures: matchingControls.map((op) => ({
            label: op.label,
            documentation: op.documentation,
            parameters: [],
          })),
          activeSignature: 0,
          activeParameter: 0,
        };
      }
    }

    return null;
  }
);

// Code Actions provider for quick fixes
connection.onCodeAction(
  async (params: CodeActionParams): Promise<CodeAction[]> => {
    let document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    let codeActions: CodeAction[] = [];
    let text = document.getText();

    // Check if formatting is enabled for format-related actions
    let settings = await getDocumentSettings(params.textDocument.uri);

    // Get diagnostics from the request
    let diagnostics = params.context.diagnostics;

    for (let diagnostic of diagnostics) {
      if (diagnostic.source === "cddl-style") {
        switch (diagnostic.code) {
          case "spacing-around-operators":
            codeActions.push(createSpacingFixAction(document, diagnostic));
            break;
          case "trailing-comma":
            codeActions.push(
              createTrailingCommaFixAction(document, diagnostic)
            );
            break;
        }
      }

      if (diagnostic.source === "cddl-lint") {
        switch (diagnostic.code) {
          case "unused-rule":
            codeActions.push(
              createRemoveUnusedRuleAction(document, diagnostic)
            );
            break;
        }
      }
    }

    // Add general formatting action only if formatting is enabled
    if (settings.formatting?.enabled) {
      codeActions.push({
        title: "Format CDDL Document",
        kind: CodeActionKind.Source,
        command: {
          title: "Format CDDL Document",
          command: "editor.action.formatDocument",
        },
      });
    }

    // Add organize imports-like action for CDDL (organize rules)
    codeActions.push({
      title: "Organize CDDL Rules",
      kind: CodeActionKind.Source,
      edit: {
        changes: {
          [params.textDocument.uri]: createOrganizeRulesEdits(document),
        },
      },
    });

    return codeActions;
  }
);

function createSpacingFixAction(
  document: TextDocument,
  diagnostic: Diagnostic
): CodeAction {
  let text = document.getText(diagnostic.range);
  let fixed = text.replace(/(\w)([=\/]{1,2}=)(\w)/, "$1 $2 $3");

  return {
    title: "Add spaces around assignment operator",
    kind: CodeActionKind.QuickFix,
    edit: {
      changes: {
        [document.uri]: [
          {
            range: diagnostic.range,
            newText: fixed,
          },
        ],
      },
    },
    diagnostics: [diagnostic],
  };
}

function createTrailingCommaFixAction(
  document: TextDocument,
  diagnostic: Diagnostic
): CodeAction {
  return {
    title: "Remove trailing comma",
    kind: CodeActionKind.QuickFix,
    edit: {
      changes: {
        [document.uri]: [
          {
            range: diagnostic.range,
            newText: "",
          },
        ],
      },
    },
    diagnostics: [diagnostic],
  };
}

function createRemoveUnusedRuleAction(
  document: TextDocument,
  diagnostic: Diagnostic
): CodeAction {
  return {
    title: "Remove unused rule",
    kind: CodeActionKind.QuickFix,
    edit: {
      changes: {
        [document.uri]: [
          {
            range: {
              start: { line: diagnostic.range.start.line, character: 0 },
              end: { line: diagnostic.range.end.line + 1, character: 0 },
            },
            newText: "",
          },
        ],
      },
    },
    diagnostics: [diagnostic],
  };
}

function createOrganizeRulesEdits(document: TextDocument): TextEdit[] {
  let text = document.getText();
  let lines = text.split("\n");

  // Simple organization: move all type rules before group rules
  // This is a basic implementation - could be enhanced
  let typeRules: string[] = [];
  let groupRules: string[] = [];
  let otherLines: string[] = [];

  for (let line of lines) {
    if (line.trim().includes(" = ")) {
      typeRules.push(line);
    } else if (line.trim().includes(" //= ")) {
      groupRules.push(line);
    } else {
      otherLines.push(line);
    }
  }

  let organized = [...otherLines, ...typeRules, ...groupRules].join("\n");

  return [
    {
      range: {
        start: { line: 0, character: 0 },
        end: document.positionAt(text.length),
      },
      newText: organized,
    },
  ];
}

// Document range formatting provider
connection.onDocumentRangeFormatting(
  async (params: DocumentRangeFormattingParams): Promise<TextEdit[]> => {
    let document = documents.get(params.textDocument.uri);
    if (!document) {
      return [];
    }

    // Check if formatting is enabled
    let settings = await getDocumentSettings(params.textDocument.uri);
    if (!settings.formatting?.enabled) {
      return [];
    }

    let text = document.getText(params.range);
    let formatted = formatCDDLText(text);

    if (formatted !== text) {
      return [
        {
          range: params.range,
          newText: formatted,
        },
      ];
    }

    return [];
  }
);

export function formatCDDLText(text: string): string {
  // Enhanced CDDL formatting rules with better map entry handling
  let lines = text.split("\n");
  let formatted: string[] = [];
  let indentLevel = 0;
  let consecutiveEmptyLines = 0;

  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    let trimmed = line.trim();

    // Handle empty lines - normalize multiple empty lines to single empty line
    if (!trimmed) {
      consecutiveEmptyLines++;
      // Only add one empty line, even if there were multiple consecutive ones
      if (consecutiveEmptyLines === 1) {
        formatted.push("");
      }
      continue;
    }

    // Reset empty line counter when we encounter content
    consecutiveEmptyLines = 0;

    // Handle map entries that are all on one line - split them
    if (
      trimmed.includes("{") &&
      trimmed.includes("}") &&
      !trimmed.endsWith("{")
    ) {
      // This is a single-line map definition, split it into multiple lines
      formatted.push(...formatSingleLineMap(trimmed, indentLevel));
      continue;
    }

    // Handle comma-separated entries within braces
    if (
      isInsideMap(trimmed) &&
      trimmed.includes(",") &&
      !trimmed.endsWith(",")
    ) {
      // Split comma-separated entries onto separate lines
      let entries = splitMapEntries(trimmed);
      for (let i = 0; i < entries.length; i++) {
        let entry = entries[i].trim();
        if (entry) {
          let indent = "  ".repeat(indentLevel);
          // Add comma to all entries except the last one (if it didn't already have one)
          if (i < entries.length - 1 && !entry.endsWith(",")) {
            entry += ",";
          }

          // Check if this entry contains a nested map and format it recursively
          if (
            entry.includes("{") &&
            entry.includes("}") &&
            !entry.endsWith("{")
          ) {
            let nestedFormatted = formatSingleLineMap(entry, indentLevel);
            // Add comma to the last line of nested map if not the last entry
            if (i < entries.length - 1 && nestedFormatted.length > 0) {
              let lastLine = nestedFormatted[nestedFormatted.length - 1];
              nestedFormatted[nestedFormatted.length - 1] = lastLine + ",";
            }
            formatted.push(...nestedFormatted);
          } else {
            formatted.push(indent + entry);
          }
        }
      }
      continue;
    }

    // Decrease indent for closing braces/brackets
    if (
      trimmed.startsWith("}") ||
      trimmed.startsWith("]") ||
      trimmed.startsWith(")")
    ) {
      indentLevel = Math.max(0, indentLevel - 1);
    }

    // Apply indentation
    let indent = "  ".repeat(indentLevel);
    formatted.push(indent + trimmed);

    // Increase indent for opening braces/brackets
    if (
      trimmed.endsWith("{") ||
      trimmed.endsWith("[") ||
      trimmed.endsWith("(")
    ) {
      indentLevel++;
    }
  }

  return formatted.join("\n");
}

function formatSingleLineMap(line: string, currentIndent: number): string[] {
  let formatted: string[] = [];
  let indent = "  ".repeat(currentIndent);

  // Extract the part before the opening brace
  let beforeBrace = line.substring(0, line.indexOf("{")).trim();
  if (beforeBrace) {
    formatted.push(indent + beforeBrace + " {");
  } else {
    formatted.push(indent + "{");
  }

  // Extract content between braces
  let content = line
    .substring(line.indexOf("{") + 1, line.lastIndexOf("}"))
    .trim();
  if (content) {
    let entries = splitMapEntries(content);
    for (let i = 0; i < entries.length; i++) {
      let entry = entries[i].trim();
      if (entry) {
        // Check if this entry contains a nested map
        if (
          entry.includes("{") &&
          entry.includes("}") &&
          !entry.endsWith("{")
        ) {
          // Recursively format nested maps
          let nestedFormatted = formatSingleLineMap(entry, currentIndent + 1);
          // Add comma to the last line of nested map if not the last entry
          if (i < entries.length - 1 && nestedFormatted.length > 0) {
            let lastLine = nestedFormatted[nestedFormatted.length - 1];
            nestedFormatted[nestedFormatted.length - 1] = lastLine + ",";
          }
          formatted.push(...nestedFormatted);
        } else {
          // Add comma to all entries except the last one (if it didn't already have one)
          if (i < entries.length - 1 && !entry.endsWith(",")) {
            entry += ",";
          }
          formatted.push("  " + indent + entry);
        }
      }
    }
  }

  formatted.push(indent + "}");
  return formatted;
}

function splitMapEntries(content: string): string[] {
  let entries: string[] = [];
  let current = "";
  let braceLevel = 0;
  let bracketLevel = 0;
  let parenLevel = 0;

  for (let i = 0; i < content.length; i++) {
    let char = content[i];

    if (char === "{") braceLevel++;
    else if (char === "}") braceLevel--;
    else if (char === "[") bracketLevel++;
    else if (char === "]") bracketLevel--;
    else if (char === "(") parenLevel++;
    else if (char === ")") parenLevel--;
    else if (
      char === "," &&
      braceLevel === 0 &&
      bracketLevel === 0 &&
      parenLevel === 0
    ) {
      entries.push(current.trim());
      current = "";
      continue;
    }

    current += char;
  }

  if (current.trim()) {
    entries.push(current.trim());
  }

  return entries;
}

function isInsideMap(line: string): boolean {
  // Simple heuristic: check if line contains key-value pairs separated by colons
  // This is not perfect but should work for most cases
  return line.includes(":") && !line.includes("=") && !line.includes("//");
}

connection.onHover((params: HoverParams): Hover | undefined => {
  // TODO: If identifier is a single character followed immediately by some
  // delimiter without any space in between, this sometimes gets tripped up
  let identifier = getIdentifierAtPosition(params);

  if (identifier === undefined) {
    return undefined;
  }

  for (const itemDetail of standardPrelude) {
    if (identifier === itemDetail.label) {
      return {
        contents: itemDetail.detail,
      };
    }
  }

  for (const itemDetail of controlOperators) {
    if (identifier == itemDetail.label) {
      return {
        contents: itemDetail.documentation
          ? (itemDetail.documentation as MarkupContent)
          : itemDetail.detail,
      };
    }
  }
});

connection.onDefinition((params: DefinitionParams) => {
  let ident = getIdentifierAtPosition(params);

  let document = documents.get(params.textDocument.uri);

  if (document === undefined) {
    return undefined;
  }

  if (ident) {
    for (const rule of cddl.rules) {
      if (rule.Type) {
        if (rule.Type.rule.name.ident === ident) {
          let start_position = document.positionAt(rule.Type.rule.name.span[0]);
          let end_position = document.positionAt(rule.Type.rule.name.span[1]);

          return {
            uri: params.textDocument.uri,
            range: {
              start: {
                character: start_position.character,
                line: start_position.line,
              },
              end: {
                character: end_position.character,
                line: end_position.line,
              },
            },
          };
        }

        if (rule.Type.rule.generic_param) {
          for (const gp of rule.Type.rule.generic_param.params) {
            if (gp.ident === ident) {
              let start_position = document.positionAt(gp.span[0]);
              let end_position = document.positionAt(gp.span[1]);

              return {
                uri: params.textDocument.uri,
                range: {
                  start: {
                    character: start_position.character,
                    line: start_position.line,
                  },
                  end: {
                    character: end_position.character,
                    line: end_position.line,
                  },
                },
              };
            }
          }
        }
      }

      if (rule.Group && rule.Group.rule.name.ident === ident) {
        let start_position = document.positionAt(rule.Group.rule.name.span[0]);
        let end_position = document.positionAt(rule.Group.rule.name.span[1]);

        return {
          uri: params.textDocument.uri,
          range: {
            start: {
              character: start_position.character,
              line: start_position.line,
            },
            end: {
              character: end_position.character,
              line: end_position.line,
            },
          },
        };
      }
    }
  }

  return undefined;
});

connection.onDocumentFormatting(
  async (params: DocumentFormattingParams): Promise<TextEdit[] | undefined> => {
    let document = documents.get(params.textDocument.uri);

    if (document === undefined) {
      return undefined;
    }

    // Check if formatting is enabled
    let settings = await getDocumentSettings(params.textDocument.uri);
    if (!settings.formatting?.enabled) {
      return [];
    }

    let formatted_text = "";
    let originalText = document.getText();

    try {
      // Preserve intentional blank lines by marking them
      let textWithMarkers = preserveBlankLines(originalText);

      // Try WASM formatter first
      formatted_text = wasm.format_cddl_from_str(textWithMarkers);

      // Restore the blank lines and apply additional formatting
      formatted_text = restoreBlankLines(formatted_text);
      formatted_text = formatCDDLText(formatted_text);
    } catch (e) {
      console.error(
        "WASM formatter failed, falling back to TypeScript formatter:",
        e
      );

      // Fall back to TypeScript formatter
      formatted_text = formatCDDLText(originalText);
    }

    // Only return edit if text actually changed
    if (formatted_text !== originalText) {
      return [
        TextEdit.replace(
          {
            start: Position.create(0, 0),
            end: document.positionAt(originalText.length),
          },
          formatted_text
        ),
      ];
    }

    return [];
  }
);

function getIdentifierAtPosition(
  docParams: TextDocumentPositionParams
): string | undefined {
  let document = documents.get(docParams.textDocument.uri);

  if (document === undefined) {
    return undefined;
  }

  let documentText = document.getText();
  let offset = document.offsetAt(docParams.position);

  if (offset === undefined) {
    return undefined;
  }

  let start = offset;
  let end = offset;

  if (
    documentText &&
    (documentText.length < offset || documentText[offset] === " ")
  ) {
    return undefined;
  }

  // CDDL identifiers can contain letters, digits, hyphens, and underscores
  let identifierChar = /[a-zA-Z0-9_-]/;

  // Find start of identifier
  while (start > 0 && identifierChar.test(documentText[start - 1])) {
    start--;
  }

  // Find end of identifier
  while (end < documentText.length && identifierChar.test(documentText[end])) {
    end++;
  }

  return documentText?.substring(start, end);
}

// Workspace Symbol Provider for searching across all CDDL files
connection.onWorkspaceSymbol(
  (params: WorkspaceSymbolParams): SymbolInformation[] => {
    let symbols: SymbolInformation[] = [];
    let query = params.query.toLowerCase();

    // Get all open documents
    let allDocuments = documents.all();

    for (let document of allDocuments) {
      // Only process CDDL files
      if (!document.uri.endsWith(".cddl")) {
        continue;
      }

      try {
        let text = document.getText();
        let fileCddl = wasm.cddl_from_str(text);

        if (fileCddl && fileCddl.rules) {
          let lines = text.split("\n");

          for (let rule of fileCddl.rules) {
            let ruleName = "";
            let kind = SymbolKind.Function;

            if (rule.Group) {
              ruleName = rule.Group.rule.name.ident;
              kind = SymbolKind.Function;
            } else if (rule.Type) {
              ruleName = rule.Type.rule.name.ident;
              kind = SymbolKind.Function;
            }

            if (ruleName && ruleName.toLowerCase().includes(query)) {
              let range = findRuleRange(ruleName, lines);

              symbols.push({
                name: ruleName,
                kind: kind,
                location: {
                  uri: document.uri,
                  range: range,
                },
                containerName:
                  document.uri.split("/").pop()?.replace(".cddl", "") || "CDDL",
              });
            }
          }
        }
      } catch (e) {
        // Skip files with parse errors
        continue;
      }
    }

    // Also search in file system if we have workspace folders
    // This would require additional file system access which we'll skip for now
    // but could be added using connection.workspace.getWorkspaceFolders()

    return symbols;
  }
);

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
