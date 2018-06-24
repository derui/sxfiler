(** Utilities for js_of_ocaml. In world of js_of_ocaml do not compile with Core or Batteries compatibility and
    size of result, and very long compile time. So this module provides small functions only.
*)

let flip f x y = f y x
