(** This module provides endpoint definition and implementation for Filer *)

open Abbrev

val initialize :
  ([> `Step_filer_instance     of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance of (module F.Common_step.File_list.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function for initialize procedure implementation *)

val reload_all :
  ([> `Step_filer_instance     of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance of (module F.Common_step.File_list.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function for reload_all procedure implementation *)

val move_location :
  ([> `Step_filer_instance     of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance of (module F.Common_step.File_list.Instance) S.Context.t
   | `Step_common_instance    of (module F.Common_step.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function for move_location procedure implementation *)

val open_node :
  ([> `Step_filer_instance     of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance of (module F.Common_step.File_list.Instance) S.Context.t
   | `Step_common_instance    of (module F.Common_step.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function for open_node work flow implementation *)

val up_directory :
  ([> `Step_filer_instance     of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance of (module F.Common_step.File_list.Instance) S.Context.t
   | `Step_common_instance    of (module F.Common_step.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function to up directory of current directory *)

val toggle_mark :
  ([> `Step_filer_instance     of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance of (module F.Common_step.File_list.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function to toggle mark of item *)

val move :
  ([> `Step_filer_instance       of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance   of (module F.Common_step.File_list.Instance) S.Context.t
   | `Step_common_instance      of (module F.Common_step.Instance) S.Context.t
   | `Step_interaction_instance of (module F.Common_step.Interaction.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function to move items *)

val copy :
  ([> `Step_filer_instance       of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance   of (module F.Common_step.File_list.Instance) S.Context.t
   | `Step_common_instance      of (module F.Common_step.Instance) S.Context.t
   | `Step_interaction_instance of (module F.Common_step.Interaction.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function to copy items *)

val delete :
  ([> `Step_filer_instance       of (module F.Common_step.Filer.Instance) S.Context.t
   | `Step_file_list_instance   of (module F.Common_step.File_list.Instance) S.Context.t
   | `Step_common_instance      of (module F.Common_step.Instance) S.Context.t
   | `Step_interaction_instance of (module F.Common_step.Interaction.Instance) S.Context.t
   ] ->
  S.Context.value) ->
  Endpoint.t
(** The function to delete items *)
