import * as monaco from 'monaco-editor';

window.editor = monaco.editor.create(
  document.getElementById('cddlDataDefinition'),
  {
    minimap: {
      enabled: false,
    },
  }
);
