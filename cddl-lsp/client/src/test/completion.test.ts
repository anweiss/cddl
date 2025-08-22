import * as vscode from "vscode";
import * as assert from "assert";
import { getDocUri, activate } from "./helper";

suite("Should do completion", () => {
  const emptyDocUri = getDocUri("empty.cddl");
  const regexpDocUri = getDocUri("regexp.cddl");

  test("Completes CDDL in empty file", async () => {
    await testCompletion(emptyDocUri, new vscode.Position(0, 0), {
      items: [{ label: "any", kind: vscode.CompletionItemKind.TypeParameter }],
    });
  });

  test("No errors for .regexp control operator", async () => {
    await testRegexpSyntax(regexpDocUri);
  });
});

async function testCompletion(
  docUri: vscode.Uri,
  position: vscode.Position,
  expectedCompletionList: vscode.CompletionList
) {
  await activate(docUri);

  // Executing the command `vscode.executeCompletionItemProvider` to simulate triggering completion
  const actualCompletionList = (await vscode.commands.executeCommand(
    "vscode.executeCompletionItemProvider",
    docUri,
    position
  )) as vscode.CompletionList;

  assert.ok(actualCompletionList.items.length >= 2);

  // Find the 'any' item instead of assuming it's at index 0
  const anyItem = actualCompletionList.items.find(
    (item) => item.label === "any"
  );
  assert.ok(anyItem, 'Should find "any" in completion items');
  assert.equal(anyItem.kind, vscode.CompletionItemKind.TypeParameter);
}

async function testRegexpSyntax(docUri: vscode.Uri) {
  await activate(docUri);

  // Wait a bit for diagnostics to be processed
  await new Promise((resolve) => setTimeout(resolve, 2000));

  // Get diagnostics for the document
  const diagnostics = vscode.languages.getDiagnostics(docUri);

  // The .regexp control operator should not produce any syntax errors
  // Filter out any non-syntax related diagnostics
  const syntaxErrors = diagnostics.filter(
    (d) => d.source === "cddl" && d.severity === vscode.DiagnosticSeverity.Error
  );

  assert.equal(
    syntaxErrors.length,
    0,
    `Expected no syntax errors for .regexp, but got: ${syntaxErrors
      .map((d) => d.message)
      .join(", ")}`
  );
}
