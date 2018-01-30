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

(** Empty key status. this have key that is empty string, and all modifiers are disabled.  *)
val empty: t

(** Convert from key combination to key status. Return None if key combination is invalid format. *)
val of_keyseq : string -> t option
