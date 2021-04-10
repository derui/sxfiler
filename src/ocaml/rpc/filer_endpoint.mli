(** This module provides endpoint definition and implementation for Filer *)

open Abbrev

val initialize : 'a F.Filer.Initialize.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function for initialize procedure implementation *)

val reload_all : 'a F.Filer.Reload_all.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function for reload_all procedure implementation *)

val move_location : 'a F.Filer.Move_location.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function for move_location procedure implementation *)

val open_node : 'a F.Filer.Open_node.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function for open_node work flow implementation *)

val up_directory : 'a F.Filer.Up_directory.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function to up directory of current directory *)

val toggle_mark : 'a F.Filer.Toggle_mark.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function to toggle mark of item *)

val move : 'a F.Filer.Move.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function to move items *)

val copy : 'a F.Filer.Copy.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function to copy items *)

val delete : 'a F.Filer.Delete.work_flow -> ('a -> S.Context.value) -> Endpoint.t
(** The function to delete items *)
