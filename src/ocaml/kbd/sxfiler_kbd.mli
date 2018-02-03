(**
   Define key combination converter for OCaml and JavaScript.

   You can use this library in OCaml, and in JavaScript follows.

   {[
     const {kbd} = require('sxfiler_kbd.bc.js');
     let obj = kbd('C-x');
       console.log(obj); // => {key: "x", ctrl: true, shift: false, meta: false}
   ]}
*)


(** A type represent key combination from string that like Emacs's [kbd] macro. *)
type t = {
  shift : bool;
  ctrl : bool;
  meta : bool;
  key : string;
}

class type js = object
  method shift: bool Js.t Js.readonly_prop
  method ctrl: bool Js.t Js.readonly_prop
  method meta: bool Js.t Js.readonly_prop
  method key: Js.js_string Js.t Js.readonly_prop
end

(** Empty key status. this have key that is empty string, and all modifiers are disabled.  *)
val empty: t

(** Convert from key combination to key status. Return None if key combination is invalid format. *)
val of_keyseq : string -> t option

(** Convert between JavaScript object and type *)
val to_js: t -> js Js.t
val of_js: js Js.t -> t
