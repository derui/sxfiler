open Sxfiler_domain
(** Completer provides simple completion interface via string. *)

val make : migemo:Migemocaml.Migemo.t -> (module Completer.Instance)
(** Get new instance of completer with migemo that is used to search Japanese. *)
