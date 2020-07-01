(** This module provides endpoint definitions and implementations for Theme *)

open Abbrev

type add_theme = F.Theme.Add_theme.work_flow -> Endpoint.t

val add_theme : add_theme
(** endpoint to add theme *)

type remove_theme = F.Theme.Remove_theme.work_flow -> Endpoint.t

val remove_theme : remove_theme
(** endpoint to remove theme *)

type list = F.Theme.List_theme.work_flow -> Endpoint.t

val list : list
(** Get all themes *)
