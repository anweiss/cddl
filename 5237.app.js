"use strict";(self.webpackChunkcddl=self.webpackChunkcddl||[]).push([[5237],{35237:(e,n,t)=>{t.a(e,(async(e,r)=>{try{t.r(n);var o=t(8560),c=e([o]);o=(c.then?(await c)():c)[0],document.getElementById("compile").addEventListener("click",(e=>{e.preventDefault();try{o.rS(window.editor.getValue()),document.getElementById("result").innerHTML='<div class="alert alert-success" role="alert">Success</div>'}catch(e){let n="";for(let t of e)n+=t.msg.short+"\n\n";document.getElementById("result").innerHTML='<div class="alert alert-danger" role="alert"><pre>'+n+"</pre></div>"}})),r()}catch(e){r(e)}}))},8560:(e,n,t)=>{t.a(e,(async(e,r)=>{try{t.d(n,{rS:()=>c.rS});var o=t(22309),c=t(14684),_=e([o]);o=(_.then?(await _)():_)[0],(0,c.lI)(o),o.__wbindgen_start(),r()}catch(e){r(e)}}))},14684:(e,n,t)=>{let r;function o(e){r=e}t.d(n,{BG:()=>S,M2:()=>h,NJ:()=>p,Oy:()=>m,QR:()=>k,Qg:()=>E,Qn:()=>Q,Rj:()=>T,bL:()=>B,ce:()=>I,lI:()=>o,p8:()=>v,qm:()=>x,rS:()=>w,v:()=>y,yc:()=>L});let c=0,_=null;function i(){return null!==_&&0!==_.byteLength||(_=new Uint8Array(r.memory.buffer)),_}let u=new("undefined"==typeof TextEncoder?(0,module.require)("util").TextEncoder:TextEncoder)("utf-8");const d="function"==typeof u.encodeInto?function(e,n){return u.encodeInto(e,n)}:function(e,n){const t=u.encode(e);return n.set(t),{read:e.length,written:t.length}};function l(e,n,t){if(void 0===t){const t=u.encode(e),r=n(t.length,1)>>>0;return i().subarray(r,r+t.length).set(t),c=t.length,r}let r=e.length,o=n(r,1)>>>0;const _=i();let l=0;for(;l<r;l++){const n=e.charCodeAt(l);if(n>127)break;_[o+l]=n}if(l!==r){0!==l&&(e=e.slice(l)),o=t(o,r,r=l+3*e.length,1)>>>0;const n=i().subarray(o+l,o+r);l+=d(e,n).written,o=t(o,r,l,1)>>>0}return c=l,o}let f=null;function a(){return(null===f||!0===f.buffer.detached||void 0===f.buffer.detached&&f.buffer!==r.memory.buffer)&&(f=new DataView(r.memory.buffer)),f}let s=new("undefined"==typeof TextDecoder?(0,module.require)("util").TextDecoder:TextDecoder)("utf-8",{ignoreBOM:!0,fatal:!0});function b(e,n){return e>>>=0,s.decode(i().subarray(e,e+n))}function g(e){const n=r.__wbindgen_export_2.get(e);return r.__externref_table_dealloc(e),n}function w(e){const n=l(e,r.__wbindgen_malloc,r.__wbindgen_realloc),t=c,o=r.cddl_from_str(n,t);if(o[2])throw g(o[1]);return g(o[0])}function m(e,n){const t=l(String(n),r.__wbindgen_malloc,r.__wbindgen_realloc),o=c;a().setInt32(e+4,o,!0),a().setInt32(e+0,t,!0)}function y(){return new Object}function h(){return new Array}function v(e,n,t){e[n>>>0]=t}function p(e,n,t){e[n]=t}function x(e){return+e}function I(e){return e}function E(e){return BigInt.asUintN(64,e)}function T(e,n){return new Error(b(e,n))}function B(){const e=r.__wbindgen_export_2,n=e.grow(4);e.set(0,void 0),e.set(n+0,void 0),e.set(n+1,null),e.set(n+2,!0),e.set(n+3,!1)}function S(e,n){return e===n}function k(e){return e}function L(e,n){return b(e,n)}function Q(e,n){throw new Error(b(e,n))}s.decode()},22309:(e,n,t)=>{var r=t(14684);e.exports=t.v(n,e.id,"49361edad78d9e2ec6c0",{"./cddl_bg.js":{__wbindgen_string_new:r.yc,__wbindgen_jsval_eq:r.BG,__wbindgen_as_number:r.qm,__wbg_String_8f0eb39a4a4c2f66:r.Oy,__wbindgen_error_new:r.Rj,__wbindgen_number_new:r.QR,__wbindgen_bigint_from_i64:r.ce,__wbindgen_bigint_from_u64:r.Qg,__wbg_set_3f1d0b984ed272ed:r.NJ,__wbg_new_78feb108b6472713:r.M2,__wbg_new_405e22f390576ce2:r.v,__wbg_set_37837023f3d740e8:r.p8,__wbindgen_throw:r.Qn,__wbindgen_init_externref_table:r.bL}})}}]);