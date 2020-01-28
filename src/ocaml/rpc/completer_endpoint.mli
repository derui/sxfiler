(** This module provides endpoint definitions and implementations for Configuration *)

open Abbrev

type initialize = F.Completer.Initialize.work_flow -> Endpoint.t

val initialize : initialize
(** Initialize collection to complete *)

type complete = F.Completer.Complete.work_flow -> Endpoint.t

val complete : complete
(** Complete with input from collection initialized before *)
