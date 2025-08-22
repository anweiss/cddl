import * as vscode from "vscode";
import * as assert from "assert";
import { getDocUri, activate } from "./helper";

suite("Should do completion", () => {
  const emptyDocUri = getDocUri("empty.cddl");

  test("Completes CDDL in empty file", async () => {
    await testCompletion(emptyDocUri, new vscode.Position(0, 0), {
      items: [{ label: "any", kind: vscode.CompletionItemKind.TypeParameter }],
    });
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
