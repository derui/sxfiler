module Wrap : Core_intf.S

module Wrap2 : Core_intf.S2

val wrap : ('a -> 'b) -> 'a Wrap.t * ('a -> 'b)
(** [wrap f] is shortcut to call {!Wrap.wrap} *)

val wrap2 : ('a -> 'b -> 'c) -> ('a, 'b) Wrap2.t * ('a -> 'b -> 'c)
(** [wrap2 f] is shortcut to call {!Wrap2.wrap} *)
