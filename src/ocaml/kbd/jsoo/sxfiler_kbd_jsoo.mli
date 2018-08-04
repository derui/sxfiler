(**
   Define key combination converter for OCaml and JavaScript.

   You can use this library in OCaml, and in JavaScript follows.

   {[
     const {kbd} = require('sxfiler_kbd.bc.js');
     let obj = kbd('C-x');
       console.log(obj); // => {key: "x", ctrl: true, meta: false}
   ]}
*)


class type js = object
  method ctrl: bool Js.t Js.readonly_prop
  method meta: bool Js.t Js.readonly_prop
  method key: Js.js_string Js.t Js.readonly_prop
end

(** Convert between JavaScript object and type *)
val to_js: Sxfiler_kbd.t -> js Js.t
val of_js: js Js.t -> Sxfiler_kbd.t
