(**
   Define key combination converter for OCaml and JavaScript.

   You can use this library in OCaml. Use {!Sxfiler_kbd_jsoo} if you use js_of_ocaml.
*)

(** A type represent key combination from string that like Emacs's [kbd] macro. *)
type t = {
  ctrl : bool;
  meta : bool;
  key : string;
}

(** Empty key status. this have key that is empty string, and all modifiers are disabled.  *)
val empty: t

(** Convert from key combination to key status. Return None if key combination is invalid format. *)
val of_keyseq: string -> t option

(** Convert from key status to key sequence. Notice when you use combination of_keyseq and to_keyseq,
    results are not equals.

    Specification of key sequence that result of this function is below.
    - sequence of meta-key should be Meta(M-),Ctrl(C-)
    - and add key
*)
val to_keyseq: t -> string
