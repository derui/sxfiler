(** This module provides simple functions for more functional style and to fill the gap of OCaml's stdlib. *)

let ident : 'a -> 'a = fun v -> v

let bracket ~setup ~teardown f =
  let data = setup () in
  (try f data
   with _ as e ->
     teardown data;
     raise e);
  teardown data

let flip f x y = f y x

let const x _ = x

let pipe f g x = g (f x)

let compose f g x = f (g x)

module Infix = struct
  let ( %> ) = pipe

  let ( % ) = compose

  let ( & ) = ( @@ )
end
