import * as monaco from 'monaco-editor';

console.log('Editor.js loaded, creating Monaco editor...');

// Configure Monaco Editor theme and options
window.editor = monaco.editor.create(
  document.getElementById('cddlDataDefinition'),
  {
    language: 'plaintext', // We can enhance this later with CDDL syntax highlighting
    theme: 'vs', // Clean light theme
    minimap: {
      enabled: false,
    },
    scrollBeyondLastLine: false,
    fontSize: 14,
    lineHeight: 20,
    fontFamily: "'Monaco', 'Menlo', 'Consolas', monospace",
    renderLineHighlight: 'line',
    cursorBlinking: 'smooth',
    cursorSmoothCaretAnimation: true,
    smoothScrolling: true,
    wordWrap: 'on',
    wrappingIndent: 'indent',
    automaticLayout: true,
    padding: {
      top: 16,
      bottom: 16,
    },
    lineNumbers: 'on',
    glyphMargin: false,
    folding: true,
    lineDecorationsWidth: 0,
    lineNumbersMinChars: 3,
    renderWhitespace: 'boundary',
    bracketPairColorization: {
      enabled: true,
    },
    guides: {
      bracketPairs: 'active',
      indentation: true,
    },
    suggest: {
      showKeywords: true,
      showSnippets: true,
    },
    quickSuggestions: {
      other: true,
      comments: false,
      strings: false,
    },
    acceptSuggestionOnCommitCharacter: true,
    acceptSuggestionOnEnter: 'on',
    accessibilitySupport: 'auto',
  }
);

console.log('Monaco editor created:', window.editor);

// Add some basic CDDL-related autocomplete suggestions
monaco.languages.registerCompletionItemProvider('plaintext', {
  provideCompletionItems: (model, position) => {
    const suggestions = [
      {
        label: 'tstr',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'tstr',
        documentation: 'Text string'
      },
      {
        label: 'bstr',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'bstr',
        documentation: 'Byte string'
      },
      {
        label: 'uint',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'uint',
        documentation: 'Unsigned integer'
      },
      {
        label: 'int',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'int',
        documentation: 'Integer'
      },
      {
        label: 'float',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'float',
        documentation: 'Floating point number'
      },
      {
        label: 'bool',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'bool',
        documentation: 'Boolean'
      },
      {
        label: 'null',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'null',
        documentation: 'Null value'
      },
      {
        label: 'any',
        kind: monaco.languages.CompletionItemKind.Keyword,
        insertText: 'any',
        documentation: 'Any value'
      },
      {
        label: '?',
        kind: monaco.languages.CompletionItemKind.Operator,
        insertText: '? ',
        documentation: 'Optional occurrence'
      },
      {
        label: '*',
        kind: monaco.languages.CompletionItemKind.Operator,
        insertText: '* ',
        documentation: 'Zero or more occurrences'
      },
      {
        label: '+',
        kind: monaco.languages.CompletionItemKind.Operator,
        insertText: '+ ',
        documentation: 'One or more occurrences'
      }
    ];

    return { suggestions };
  }
});
