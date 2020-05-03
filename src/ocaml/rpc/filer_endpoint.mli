(** This module provides endpoint definition and implementation for Filer *)

open Abbrev

type initialize = F.Filer.Initialize.work_flow -> Endpoint.t

val initialize : initialize
(** The function for initialize procedure implementation *)

type reload_all = F.Common_step.Filer.get -> F.Filer.Reload_all.work_flow -> Endpoint.t

val reload_all : reload_all
(** The function for reload_all procedure implementation *)

type move_location = F.Common_step.Filer.get -> F.Filer.Move_location.work_flow -> Endpoint.t

val move_location : move_location
(** The function for move_location procedure implementation *)

type open_node = F.Common_step.Filer.get -> F.Filer.Open_node.work_flow -> Endpoint.t

val open_node : open_node
(** The function for open_node work flow implementation *)

type up_directory = F.Common_step.Filer.get -> F.Filer.Up_directory.work_flow -> Endpoint.t

val up_directory : up_directory
(** The function to up directory of current directory *)

type toggle_mark = F.Common_step.Filer.get -> F.Filer.Toggle_mark.work_flow -> Endpoint.t

val toggle_mark : toggle_mark
(** The function to toggle mark of item *)

type move = F.Common_step.Filer.get -> F.Filer.Move.work_flow -> Endpoint.t

val move : move
(** The function to move items *)
