open Sxfiler_domain
(** Completer provides simple completion interface via string. *)

include module type of struct
  include Completer
end

val make : migemo:Migemocaml.Migemo.t -> (module Instance)
(** Get new instance of completer with migemo that is used to search Japanese. *)
