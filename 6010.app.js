"use strict";(self.webpackChunkcddl=self.webpackChunkcddl||[]).push([[6010],{92657:(n,e,t)=>{t.a(n,(async(n,r)=>{try{t.d(e,{LM:()=>c.LM});var o=t(62993),c=t(18846),i=n([o]);o=(i.then?(await i)():i)[0],(0,c.oT)(o),r()}catch(n){r(n)}}))},18846:(n,e,t)=>{let r;function o(n){r=n}t.d(e,{$R:()=>q,Jm:()=>B,Kx:()=>D,LM:()=>p,Or:()=>O,WD:()=>M,Yq:()=>x,al:()=>A,h4:()=>T,hd:()=>E,m9:()=>k,m_:()=>L,n0:()=>j,oT:()=>o,pT:()=>I,ug:()=>v}),n=t.hmd(n);const c=new Array(128).fill(void 0);function i(n){return c[n]}c.push(void 0,null,!0,!1);let _=c.length;function u(n){const e=i(n);return function(n){n<132||(c[n]=_,_=n)}(n),e}let d=new("undefined"==typeof TextDecoder?(0,n.require)("util").TextDecoder:TextDecoder)("utf-8",{ignoreBOM:!0,fatal:!0});d.decode();let l=null;function f(){return null!==l&&0!==l.byteLength||(l=new Uint8Array(r.memory.buffer)),l}function a(n,e){return n>>>=0,d.decode(f().subarray(n,n+e))}function s(n){_===c.length&&c.push(c.length+1);const e=_;return _=c[e],c[e]=n,e}let g=0,w=new("undefined"==typeof TextEncoder?(0,n.require)("util").TextEncoder:TextEncoder)("utf-8");const b="function"==typeof w.encodeInto?function(n,e){return w.encodeInto(n,e)}:function(n,e){const t=w.encode(n);return e.set(t),{read:n.length,written:t.length}};function h(n,e,t){if(void 0===t){const t=w.encode(n),r=e(t.length,1)>>>0;return f().subarray(r,r+t.length).set(t),g=t.length,r}let r=n.length,o=e(r,1)>>>0;const c=f();let i=0;for(;i<r;i++){const e=n.charCodeAt(i);if(e>127)break;c[o+i]=e}if(i!==r){0!==i&&(n=n.slice(i)),o=t(o,r,r=i+3*n.length,1)>>>0;const e=f().subarray(o+i,o+r);i+=b(n,e).written}return g=i,o}let m=null;function y(){return null!==m&&0!==m.byteLength||(m=new Int32Array(r.memory.buffer)),m}function p(n){try{const o=r.__wbindgen_add_to_stack_pointer(-16),c=h(n,r.__wbindgen_malloc,r.__wbindgen_realloc),i=g;r.cddl_from_str(o,c,i);var e=y()[o/4+0],t=y()[o/4+1];if(y()[o/4+2])throw u(t);return u(e)}finally{r.__wbindgen_add_to_stack_pointer(16)}}function v(n){u(n)}function T(n,e){return s(a(n,e))}function x(n,e){return i(n)===i(e)}function E(n,e){return s(new Error(a(n,e)))}function L(n){return s(i(n))}function k(n,e){const t=h(String(i(e)),r.__wbindgen_malloc,r.__wbindgen_realloc),o=g;y()[n/4+1]=o,y()[n/4+0]=t}function I(n){return s(n)}function M(n){return s(n)}function D(n){return s(BigInt.asUintN(64,n))}function j(n,e,t){i(n)[u(e)]=u(t)}function q(){return s(new Array)}function A(){return s(new Object)}function B(n,e,t){i(n)[e>>>0]=u(t)}function O(n,e){throw new Error(a(n,e))}},46010:(n,e,t)=>{t.a(n,(async(n,r)=>{try{t.r(e);var o=t(92657),c=n([o]);o=(c.then?(await c)():c)[0],document.getElementById("compile").addEventListener("click",(n=>{n.preventDefault();try{o.LM(window.editor.getValue()),document.getElementById("result").innerHTML='<div class="alert alert-success" role="alert">Success</div>'}catch(n){let e="";for(let t of n)e+=t.msg.short+"\n\n";document.getElementById("result").innerHTML='<div class="alert alert-danger" role="alert"><pre>'+e+"</pre></div>"}})),r()}catch(n){r(n)}}))},62993:(n,e,t)=>{var r=t(18846);n.exports=t.v(e,n.id,"10d16db885578f120abe",{"./cddl_bg.js":{__wbindgen_object_drop_ref:r.ug,__wbindgen_string_new:r.h4,__wbindgen_jsval_eq:r.Yq,__wbindgen_error_new:r.hd,__wbindgen_object_clone_ref:r.m_,__wbg_String_88810dfeb4021902:r.m9,__wbindgen_number_new:r.pT,__wbindgen_bigint_from_i64:r.WD,__wbindgen_bigint_from_u64:r.Kx,__wbg_set_841ac57cff3d672b:r.n0,__wbg_new_ffc6d4d085022169:r.$R,__wbg_new_9fb8d994e1c0aaac:r.al,__wbg_set_f2740edb12e318cd:r.Jm,__wbindgen_throw:r.Or}})}}]);