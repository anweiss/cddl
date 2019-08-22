import * as wasm from "cddl";


document.getElementById('compile').addEventListener('click', () => {
  event.preventDefault();

  try {
    wasm.compile_cddl_from_str(document.getElementById('cddlDataDefinition').value);
    document.getElementById('result').innerHTML = '<div class="alert alert-success" role="alert">Success</div>';
  } catch (err) {
    document.getElementById('result').innerHTML = '<div class="alert alert-danger" role="alert">Error: ' + err + '</div>';
  }
});