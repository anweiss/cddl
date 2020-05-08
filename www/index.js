import * as wasm from 'cddl';

document.getElementById('compile').addEventListener('click', (e) => {
  e.preventDefault();

  try {
    let cddl = wasm.cddl_from_str(window.editor.getValue());
    document.getElementById('result').innerHTML =
      '<div class="alert alert-success" role="alert">Success</div>';
  } catch (errors) {
    let errorMsg = '';

    for (let err of errors) {
      errorMsg += err.msg.short + '\n\n';
    }

    document.getElementById('result').innerHTML =
      '<div class="alert alert-danger" role="alert"><pre>' +
      errorMsg +
      '</pre></div>';
  }
});
