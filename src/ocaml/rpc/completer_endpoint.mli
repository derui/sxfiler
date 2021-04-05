(** This module provides endpoint definitions and implementations for Configuration *)

open Abbrev

val initialize :
  ([> `Step_completer_instance of (module F.Common_step.Completer.Instance) S.Context.t ] -> S.Context.value) ->
  Endpoint.t
(** Initialize collection to complete *)

val complete :
  ([> `Step_completer_instance of (module F.Common_step.Completer.Instance) S.Context.t
   | `Completer_instance      of (module D.Completer.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** Complete with input from collection initialized before *)
