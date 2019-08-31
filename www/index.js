import * as wasm from "cddl";

document.getElementById('compile').addEventListener('click', e => {
  e.preventDefault();

  try {
    wasm.compile_cddl_from_str(window.editor.getValue());
    document.getElementById('result').innerHTML = '<div class="alert alert-success" role="alert">Success</div>';
  } catch (err) {
    document.getElementById('result').innerHTML = '<div class="alert alert-danger" role="alert"><pre>' + err + '</pre></div>';
  }
});