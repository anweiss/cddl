import * as vscode from "vscode";
import * as assert from "assert";
import { getDocUri, activate, replaceAndWaitForDiagnostics, replaceText } from "./helper";

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
  expectedCompletionList: vscode.CompletionList,
) {
  await activate(docUri);

  // Executing the command `vscode.executeCompletionItemProvider` to simulate triggering completion
  const actualCompletionList = (await vscode.commands.executeCommand(
    "vscode.executeCompletionItemProvider",
    docUri,
    position,
  )) as vscode.CompletionList;

  for (const expected of expectedCompletionList.items) {
    const actual = actualCompletionList.items.find((item) => item.label === expected.label);
    assert.ok(actual, `Missing completion ${String(expected.label)}`);
    assert.equal(actual.kind, expected.kind);
  }
}

async function testRegexpSyntax(docUri: vscode.Uri) {
  const { doc, editor } = await activate(docUri);
  const original = doc.getText();
  try {
    await replaceAndWaitForDiagnostics(editor, "root = {", (items) =>
      items.some((item) => item.severity === vscode.DiagnosticSeverity.Error),
    );
    const diagnostics = await replaceAndWaitForDiagnostics(editor, original, (items) =>
      items.every((item) => item.severity !== vscode.DiagnosticSeverity.Error),
    );

    // The .regexp control operator should not produce any syntax errors
    // Filter out any non-syntax related diagnostics
    const syntaxErrors = diagnostics.filter(
      (d) => d.source === "cddl" && d.severity === vscode.DiagnosticSeverity.Error,
    );

    assert.equal(
      syntaxErrors.length,
      0,
      `Expected no syntax errors for .regexp, but got: ${syntaxErrors
        .map((d) => d.message)
        .join(", ")}`,
    );
  } finally {
    if (doc.getText() !== original) await replaceText(editor, original);
  }
}
