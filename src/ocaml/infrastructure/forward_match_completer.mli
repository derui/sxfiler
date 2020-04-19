open Sxfiler_domain
(** Completer provides simple completion interface via string. *)

val make : unit -> (module Completer.Instance)
(** Get new instance of completer with forward match. *)
