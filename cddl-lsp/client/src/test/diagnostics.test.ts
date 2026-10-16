import * as vscode from "vscode";
import * as assert from "assert";
import { activate, getDocUri, replaceAndWaitForDiagnostics, replaceText } from "./helper";

suite("CDDL diagnostics", () => {
  test("Publishes syntax errors and clears them after correction", async () => {
    const { doc, editor } = await activate(getDocUri("diagnostics.cddl"));
    const original = doc.getText();
    try {
      const errors = await replaceAndWaitForDiagnostics(editor, "root = {", (diagnostics) =>
        diagnostics.some((item) => item.severity === vscode.DiagnosticSeverity.Error),
      );
      assert.ok(errors.length > 0);
      assert.ok(errors.every((item) => item.range.start.line >= 0));
      const cleared = await replaceAndWaitForDiagnostics(
        editor,
        original,
        (items) => items.length === 0,
      );
      assert.deepStrictEqual(cleared, []);
    } finally {
      if (doc.getText() !== original) await replaceText(editor, original);
    }
  }).timeout(20000);
});
