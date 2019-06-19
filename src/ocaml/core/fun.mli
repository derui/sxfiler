(** This module provides simple functions for more functional style and to fill the gap of OCaml's
    stdlib. *)

val ident : 'a -> 'a
(** [ident v] return same value given. This is known as {b id} in Haskell. *)

val bracket : setup:(unit -> 'a) -> teardown:('a -> unit) -> ('a -> unit) -> unit
(** [bracket ~setup ~teardown f] get data from [setup] and run [f] with it, and finalize [teardown]
    always. if [f] raise exception, do finalize and re-raise it. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** [flip f x y] get flipped function. *)

val const : 'a -> 'b -> 'a
(** [const x y] gets constant function to return [x] always called. *)

val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [f %> g] compose [f] and [g] to call as pipeline flow these. *)

val ( %< ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** [f %< g] compose [f] and [g] as [f (g v)] *)

val ( & ) : ('a -> 'b) -> 'a -> 'b
(** [f & v] shortcut as {!\@\@} *)
