(window["webpackJsonp"] = window["webpackJsonp"] || []).push([[8],{

/***/ "./editor.js":
/*!*******************!*\
  !*** ./editor.js ***!
  \*******************/
/*! no exports provided */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony import */ var monaco_editor__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! monaco-editor */ \"./node_modules/monaco-editor/esm/vs/editor/editor.main.js\");\n\n\n// self.MonacoEnvironment = {\n// \tgetWorkerUrl: function (moduleId, label) {\n// \t\tif (label === 'json') {\n// \t\t\treturn './json.worker.bundle.js';\n// \t\t}\n// \t\tif (label === 'css') {\n// \t\t\treturn './css.worker.bundle.js';\n// \t\t}\n// \t\tif (label === 'html') {\n// \t\t\treturn './html.worker.bundle.js';\n// \t\t}\n// \t\tif (label === 'typescript' || label === 'javascript') {\n// \t\t\treturn './ts.worker.bundle.js';\n// \t\t}\n// \t\treturn './editor.worker.bundle.js';\n// \t}\n// }\n\nwindow.editor = monaco_editor__WEBPACK_IMPORTED_MODULE_0__[\"editor\"].create(document.getElementById('cddlDataDefinition'), {\n  minimap: {\n    enabled: false\n  }\n});\n\n//# sourceURL=webpack:///./editor.js?");

/***/ })

}]);