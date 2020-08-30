(** This module provides endpoint definitions and implementations for Theme *)

open Abbrev

type get = F.Theme.Get_theme.work_flow -> Endpoint.t

val get : get
(** endpoint to get theme *)

type update = F.Theme.Update_theme.work_flow -> Endpoint.t

val update : update
(** Update theme configuration *)
