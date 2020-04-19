open Sxfiler_workflow
(** This module provide implementation of steps of filer that are needed IO in implementation *)

val scan_location : Common_step.File_list.scan_location
(** implementations for [Common_step_file_list.scan_location] with real IO *)

val copy_item : Common_step.Filer.copy_item
(** implementations for [Common_step_filer.copy_item] with real IO *)

val move_item : Common_step.Filer.move_item
(** implementations for [Common_step_filer.move_item] with real IO *)

val delete_item : Common_step.Filer.delete_item
(** implementations for [Common_step_filer.delete_item] with real IO *)
