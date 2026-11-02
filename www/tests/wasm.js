import { readFileSync } from 'node:fs';
import * as wasmModule from '../../pkg/cddl.js';

wasmModule.initSync({
  module: readFileSync(new URL('../../pkg/cddl_bg.wasm', import.meta.url)),
});

export { wasmModule };
