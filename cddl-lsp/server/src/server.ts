import {
  createConnection,
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
  let ruleDefinitions: Map<string, { line: number; range: Range }> = new Map();

  // Collect all defined rules
  for (let rule of cddl.rules) {
    if (rule.Group) {
      let ruleName = rule.Group.rule.name.ident;
      definedRules.add(ruleName);
      let range = findRuleRange(ruleName, lines);
      ruleDefinitions.set(ruleName, { line: range.start.line, range });
    }
    if (rule.Type) {
      let ruleName = rule.Type.rule.name.ident;
      definedRules.add(ruleName);
      let range = findRuleRange(ruleName, lines);
      ruleDefinitions.set(ruleName, { line: range.start.line, range });
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
          let regex = new RegExp(`\\b${ruleName}\\b`, "g");
          if (regex.test(rightSide)) {
            usedRules.add(ruleName);
          }
        }
      }
    } else {
      // For lines without assignments, check the entire line
      for (let ruleName of definedRules) {
        let regex = new RegExp(`\\b${ruleName}\\b`, "g");
        if (regex.test(line)) {
          usedRules.add(ruleName);
        }
      }
    }

    // Check for map key references (rule-name =>)
    for (let ruleName of definedRules) {
      let mapKeyRegex = new RegExp(`\\b${ruleName}\\s*=>`, "g");
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
            range: ruleInfo.range,
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
  while (errors.length < settings.maxNumberOfProblems) {
    for (const error of errors) {
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

    break;
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

          symbols.push({
            name: ruleName,
            kind: SymbolKind.Class,
            range: range,
            selectionRange: range,
            detail: "Group rule",
          });
        }
      }

      if (rule.Type) {
        let ruleName = rule.Type.rule.name.ident;
        if (!seenSymbols.has(ruleName)) {
          seenSymbols.add(ruleName);
          let range = findRuleRange(ruleName, lines);

          symbols.push({
            name: ruleName,
            kind: SymbolKind.TypeParameter,
            range: range,
            selectionRange: range,
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

// References provider for jump to definition
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

  // Find start of word
  while (start > 0 && /\w/.test(text[start - 1])) {
    start--;
  }

  // Find end of word
  while (end < text.length && /\w/.test(text[end])) {
    end++;
  }

  if (start === end) {
    return null;
  }

  return text.substring(start, end);
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

  for (let line of lines) {
    let trimmed = line.trim();

    if (!trimmed) {
      formatted.push("");
      continue;
    }

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
      // Try WASM formatter first
      formatted_text = wasm.format_cddl_from_str(originalText);

      // Apply additional formatting for better map entry handling
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

  let re = /\s|,|:|=|\*|\?|\+|<|>|{|}|\[|\]|\(|\)/;

  while (!re.test(documentText[start]) && start > 0) {
    start--;
  }
  while (!re.test(documentText[end]) && end < documentText.length) {
    end++;
  }

  return documentText?.substring(start == 0 ? 0 : start + 1, end);
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
