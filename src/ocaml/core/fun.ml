(** This module provides simple functions for more functional style and to fill the gap
    of OCaml's stdlib.  *)

(** [ident v] return same value given. This is known as {b id} in Haskell. *)
let ident: 'a -> 'a = fun v -> v

(** [bracket ~setup ~teardown f] get data from [setup] and run [f] with it, and finalize [teardown] always.
    if [f] raise exception, do finalize and re-raise it.
*)
let  bracket ~setup ~teardown f =
  let data = setup () in
  begin
    try
      f data
    with _ as e ->
      teardown data;
      raise e
  end;
  teardown data

(** [flip f x y] get flipped function. *)
let flip f x y = f y x
