"use strict";(self.webpackChunkcddl=self.webpackChunkcddl||[]).push([[6010],{18846:(n,e,t)=>{t.a(n,(async(r,c)=>{try{t.d(e,{BF:()=>D,BL:()=>M,LM:()=>I,Or:()=>U,WA:()=>C,gp:()=>S,h4:()=>A,jp:()=>E,m_:()=>k,pT:()=>L,pj:()=>j,tr:()=>x,uB:()=>O,ug:()=>T});var o=t(62993);n=t.hmd(n);var i=r([o]);o=(i.then?(await i)():i)[0];const _=new Array(32).fill(void 0);function d(n){return _[n]}_.push(void 0,null,!0,!1);let u=_.length;function a(n){n<36||(_[n]=u,u=n)}function l(n){const e=d(n);return a(n),e}let f=new("undefined"==typeof TextDecoder?(0,n.require)("util").TextDecoder:TextDecoder)("utf-8",{ignoreBOM:!0,fatal:!0});f.decode();let g=new Uint8Array;function s(){return 0===g.byteLength&&(g=new Uint8Array(o.memory.buffer)),g}function w(n,e){return f.decode(s().subarray(n,n+e))}function b(n){u===_.length&&_.push(_.length+1);const e=u;return u=_[e],_[e]=n,e}let h=0,y=new("undefined"==typeof TextEncoder?(0,n.require)("util").TextEncoder:TextEncoder)("utf-8");const p="function"==typeof y.encodeInto?function(n,e){return y.encodeInto(n,e)}:function(n,e){const t=y.encode(n);return e.set(t),{read:n.length,written:t.length}};function m(n,e,t){if(void 0===t){const t=y.encode(n),r=e(t.length);return s().subarray(r,r+t.length).set(t),h=t.length,r}let r=n.length,c=e(r);const o=s();let i=0;for(;i<r;i++){const e=n.charCodeAt(i);if(e>127)break;o[c+i]=e}if(i!==r){0!==i&&(n=n.slice(i)),c=t(c,r,r=i+3*n.length);const e=s().subarray(c+i,c+r);i+=p(n,e).written}return h=i,c}let v=new Int32Array;function B(){return 0===v.byteLength&&(v=new Int32Array(o.memory.buffer)),v}function I(n){try{const r=o.__wbindgen_add_to_stack_pointer(-16),c=m(n,o.__wbindgen_malloc,o.__wbindgen_realloc),i=h;o.cddl_from_str(r,c,i);var e=B()[r/4+0],t=B()[r/4+1];if(B()[r/4+2])throw l(t);return l(e)}finally{o.__wbindgen_add_to_stack_pointer(16)}}function T(n){l(n)}function A(n,e){return b(w(n,e))}function E(n){return b(BigInt(n))}function L(n){return b(n)}function j(n){return b(BigInt(BigInt.asUintN(64,n)))}function k(n){return b(d(n))}function x(n,e){const t=m(String(d(e)),o.__wbindgen_malloc,o.__wbindgen_realloc),r=h;B()[n/4+1]=r,B()[n/4+0]=t}function M(n,e,t){d(n)[l(e)]=l(t)}function D(){return b(new Array)}function O(){return b(new Object)}function C(n,e,t){d(n)[e>>>0]=l(t)}function S(n,e){return b(new Error(w(n,e)))}function U(n,e){throw new Error(w(n,e))}c()}catch(q){c(q)}}))},46010:(n,e,t)=>{t.a(n,(async(n,r)=>{try{t.r(e);var c=t(18846),o=n([c]);c=(o.then?(await o)():o)[0],document.getElementById("compile").addEventListener("click",(n=>{n.preventDefault();try{c.LM(window.editor.getValue()),document.getElementById("result").innerHTML='<div class="alert alert-success" role="alert">Success</div>'}catch(n){let e="";for(let t of n)e+=t.msg.short+"\n\n";document.getElementById("result").innerHTML='<div class="alert alert-danger" role="alert"><pre>'+e+"</pre></div>"}})),r()}catch(n){r(n)}}))},62993:(n,e,t)=>{t.a(n,(async(r,c)=>{try{var o,i=r([o=t(18846)]),[o]=i.then?(await i)():i;await t.v(e,n.id,"4cd5e7dc10fdbe98618c",{"./cddl_bg.js":{__wbindgen_object_drop_ref:o.ug,__wbindgen_string_new:o.h4,__wbg_BigInt_d0c7d465bfa30d3b:o.jp,__wbindgen_number_new:o.pT,__wbg_BigInt_1fab4952b6c4a499:o.pj,__wbindgen_object_clone_ref:o.m_,__wbg_String_c9c0f9be374874ba:o.tr,__wbg_set_c943d600fa71e4dd:o.BL,__wbg_new_1d9a920c6bfc44a8:o.BF,__wbg_new_0b9bfdd97583284e:o.uB,__wbg_set_a68214f35c417fa9:o.WA,__wbg_new_8d2af00bc1e329ee:o.gp,__wbindgen_throw:o.Or}}),c()}catch(n){c(n)}}),1)}}]);