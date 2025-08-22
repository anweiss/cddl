# CDDL Language Server Extension

[![](https://vsmarketplacebadge.apphb.com/version/anweiss.cddl-languageserver.svg)](https://marketplace.visualstudio.com/items?itemName=anweiss.cddl-languageserver)

> This extension is a Preview. Report bugs and issues at https://github.com/anweiss/cddl/issues.

Language server implementation and Visual Studio Code Extension for the Concise Data Definition Language (CDDL). This extension supports the following features:

* **Syntax highlighting**
* **Intellisense**
  + Standard prelude
  + Control operators
  + Group and type rule identifiers
  + Socket/plug completions with deduplication
* **Go-to definition**
* **Find references**
* **Document symbols** (outline view)
* **Workspace symbol search** (search across all CDDL files)
* **Signature help** for generics and control operators
* **Enhanced diagnostics**
  + Syntax validation
  + Unused rule detection
  + Style warnings (spacing, trailing commas)
  + Circular dependency detection
* **Code actions & quick fixes**
  + Fix spacing around operators
  + Remove trailing commas
  + Remove unused rules
  + Organize CDDL rules
  + Format document
* **Advanced formatting** (configurable)
  + Multi-line map formatting
  + Proper indentation
  + Range formatting support
  + Can be disabled via settings

## Configuration

This extension contributes the following settings:

* `cddllsp.maxNumberOfProblems`: Controls the maximum number of problems produced by the server (default: 100)
* `cddllsp.trace.server`: Traces the communication between VS Code and the language server (default: "off")
* `cddllsp.formatting.enabled`: Enable/disable CDDL document formatting (default: true)

### Disabling Formatting

To disable CDDL formatting, add this to your VS Code settings:

```json
{
  "cddllsp.formatting.enabled": false
}
```

When disabled, the "Format Document" command and automatic formatting will not modify CDDL content.
