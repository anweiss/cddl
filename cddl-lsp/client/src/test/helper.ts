import * as vscode from "vscode";
import * as path from "path";

/**
 * Activates the anweiss.cddl-languageserver extension
 */
export async function activate(docUri: vscode.Uri) {
  // The extensionId is `publisher.name` from package.json
  const ext = vscode.extensions.getExtension("anweiss.cddl-languageserver");
  if (!ext) throw new Error("CDDL extension is missing from the test host.");
  await ext.activate();
  const doc = await vscode.workspace.openTextDocument(docUri);
  if (doc.languageId !== "cddl") throw new Error("Synthetic fixture is not detected as CDDL.");
  const editor = await vscode.window.showTextDocument(doc);
  return { doc, editor };
}

export async function replaceText(editor: vscode.TextEditor, text: string): Promise<void> {
  const applied = await editor.edit((edit) => {
    edit.replace(
      new vscode.Range(
        editor.document.positionAt(0),
        editor.document.positionAt(editor.document.getText().length),
      ),
      text,
    );
  });
  if (!applied) throw new Error("Could not edit the synthetic test document.");
}

/** Subscribe before editing so fast server notifications cannot be missed. */
export function replaceAndWaitForDiagnostics(
  editor: vscode.TextEditor,
  text: string,
  accept: (diagnostics: vscode.Diagnostic[]) => boolean,
): Promise<vscode.Diagnostic[]> {
  return new Promise((resolve, reject) => {
    const uri = editor.document.uri;
    let notifications = 0;
    let observed: number[] = [];
    const timer = setTimeout(() => {
      subscription.dispose();
      reject(
        new Error(
          `Timed out waiting for CDDL diagnostics (version=${editor.document.version}, notifications=${notifications}, severities=${observed.join(",")}).`,
        ),
      );
    }, 10000);
    const subscription = vscode.languages.onDidChangeDiagnostics((event) => {
      if (!event.uris.some((changed) => changed.toString() === uri.toString())) return;
      const diagnostics = vscode.languages.getDiagnostics(uri);
      notifications++;
      observed = diagnostics.map((diagnostic) => diagnostic.severity);
      if (accept(diagnostics)) {
        clearTimeout(timer);
        subscription.dispose();
        resolve(diagnostics);
      }
    });
    replaceText(editor, text).catch((error: unknown) => {
      clearTimeout(timer);
      subscription.dispose();
      reject(error);
    });
  });
}

const getDocPath = (p: string) => {
  return path.resolve(__dirname, "../../testFixture", p);
};
export const getDocUri = (p: string) => {
  return vscode.Uri.file(getDocPath(p));
};
