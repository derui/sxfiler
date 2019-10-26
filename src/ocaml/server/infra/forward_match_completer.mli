open Sxfiler_domain
(** Completer provides simple completion interface via string. *)

include module type of struct
  include Completer
end

val make : unit -> (module Instance)
(** Get new instance of completer with forward match. *)
