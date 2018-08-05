(** Completer provides simple completion interface via string. *)
open Sxfiler_completion

include module type of (struct include Completer_intf end)

(** Get new instance of completer with migemo that is used to search Japanese. *)
val make: migemo:Migemocaml.Migemo.t -> (module Instance)
