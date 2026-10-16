import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  createProtocolConnection,
  type ProtocolConnection,
  type PublishDiagnosticsParams,
  type CompletionItem,
  type DocumentSymbol,
  type InitializeResult,
} from "vscode-languageserver/node";

let child: ChildProcessWithoutNullStreams;
let connection: ProtocolConnection;
let capabilities: InitializeResult;
const uri = "file:///synthetic-readiness-test.cddl";

beforeEach(async () => {
  child = spawn(
    process.execPath,
    [fileURLToPath(new URL("../out/server.js", import.meta.url)), "--stdio"],
    {
      stdio: "pipe",
    },
  );
  connection = createProtocolConnection(child.stdout, child.stdin);
  connection.listen();
  capabilities = await connection.sendRequest("initialize", {
    processId: process.pid,
    rootUri: null,
    capabilities: {},
  });
  await connection.sendNotification("initialized", {});
});

afterEach(async () => {
  try {
    await connection.sendRequest("shutdown");
    await connection.sendNotification("exit");
  } finally {
    connection.dispose();
    child.kill();
  }
});

async function open(text: string): Promise<PublishDiagnosticsParams> {
  const diagnostics = new Promise<PublishDiagnosticsParams>((resolve) => {
    const listener = connection.onNotification(
      "textDocument/publishDiagnostics",
      (params: PublishDiagnosticsParams) => {
        if (params.uri === uri) {
          listener.dispose();
          resolve(params);
        }
      },
    );
  });
  await connection.sendNotification("textDocument/didOpen", {
    textDocument: { uri, languageId: "cddl", version: 1, text },
  });
  return diagnostics;
}

describe("language-server stdio protocol with real WASM", () => {
  it("advertises its supported protocol capabilities", () => {
    expect(capabilities.capabilities.definitionProvider).toBe(true);
    expect(capabilities.capabilities.completionProvider?.resolveProvider).toBe(true);
  });
  it("publishes syntax diagnostics and clears them after correction", async () => {
    const invalid = await open("root = {");
    expect(invalid.diagnostics.length).toBeGreaterThan(0);
    for (const diagnostic of invalid.diagnostics) {
      expect(diagnostic.range.end.line).toBe(0);
      expect(diagnostic.range.end.character).toBeLessThanOrEqual("root = {".length);
    }
    const corrected = new Promise<PublishDiagnosticsParams>((resolve) => {
      const listener = connection.onNotification(
        "textDocument/publishDiagnostics",
        (params: PublishDiagnosticsParams) => {
          if (params.uri === uri && params.diagnostics.length === 0) {
            listener.dispose();
            resolve(params);
          }
        },
      );
    });
    await connection.sendNotification("textDocument/didChange", {
      textDocument: { uri, version: 2 },
      contentChanges: [{ text: "root = uint" }],
    });
    expect((await corrected).diagnostics).toEqual([]);
  });
  it("offers prelude completions and document symbols", async () => {
    await open("root = uint\nother = tstr");
    const items = await connection.sendRequest<CompletionItem[]>("textDocument/completion", {
      textDocument: { uri },
      position: { line: 0, character: 7 },
    });
    expect(items.some((item) => item.label === "uint")).toBe(true);
    const symbols = await connection.sendRequest<DocumentSymbol[]>("textDocument/documentSymbol", {
      textDocument: { uri },
    });
    expect(symbols.map((symbol) => symbol.name)).toEqual(expect.arrayContaining(["root", "other"]));
  });
  it("resolves a reference to its definition range", async () => {
    await open("root = other\nother = uint");
    const definition = await connection.sendRequest<{
      uri: string;
      range: { start: { line: number; character: number } };
    }>("textDocument/definition", { textDocument: { uri }, position: { line: 0, character: 9 } });
    expect(definition.uri).toBe(uri);
    expect(definition.range.start).toEqual({ line: 1, character: 0 });
  });
});
