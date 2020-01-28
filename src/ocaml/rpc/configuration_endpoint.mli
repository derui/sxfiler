(** This module provides endpoint definitions and implementations for Configuration *)

open Abbrev

(* query endpoints *)
type get = F.Common_step.Configuration.load -> Endpoint.t

val get : get
(** Query endpoint that get current key map *)
