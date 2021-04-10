(** Workflows defined by this module *)

include module type of Filer_intf

val initialize :
  Initialize.input ->
  ( event list,
    [> `Step_filer_instance     of (module Common_step_filer.Instance) S.Context.t
    | `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t
    ] )
  S.t
(** The workflow to initialize filer with scanning locations *)

val reload_all :
  Reload_all.input ->
  ( (event list, Reload_all.error) result,
    [> `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t
    | `Step_filer_instance     of (module Common_step_filer.Instance) S.Context.t
    ] )
  S.t
(** The workflow to reload file lists in both side of the filer *)

val move_location :
  Move_location.input ->
  ( (event list, Move_location.error) result,
    [> `Step_common_instance    of (module Common_step_common.Instance) S.Context.t
    | `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t
    | `Step_filer_instance     of (module Common_step_filer.Instance) S.Context.t
    ] )
  S.t
(** The workflow to move location of file list specified side of the filer *)

val copy :
  Copy.input ->
  ( (Copy.output, Copy.error) result,
    [> `Step_common_instance      of (module Common_step_common.Instance) S.Context.t
    | `Step_file_list_instance   of (module Common_step_file_list.Instance) S.Context.t
    | `Step_filer_instance       of (module Common_step_filer.Instance) S.Context.t
    | `Step_interaction_instance of (module Common_step_interaction.Instance) S.Context.t
    ] )
  S.t
(** The workflow to copy the item to other file window *)

val move :
  Move.input ->
  ( (Move.output, Move.error) result,
    [> `Step_common_instance      of (module Common_step_common.Instance) S.Context.t
    | `Step_file_list_instance   of (module Common_step_file_list.Instance) S.Context.t
    | `Step_filer_instance       of (module Common_step_filer.Instance) S.Context.t
    | `Step_interaction_instance of (module Common_step_interaction.Instance) S.Context.t
    ] )
  S.t
(** The workflow to move the item to other file window *)

val delete :
  Delete.input ->
  ( (Delete.output, Delete.error) result,
    [> `Step_common_instance      of (module Common_step_common.Instance) S.Context.t
    | `Step_file_list_instance   of (module Common_step_file_list.Instance) S.Context.t
    | `Step_filer_instance       of (module Common_step_filer.Instance) S.Context.t
    | `Step_interaction_instance of (module Common_step_interaction.Instance) S.Context.t
    ] )
  S.t

val open_node :
  Open_node.input ->
  ( (Open_node.output, Open_node.error) result,
    [> `Step_common_instance    of (module Common_step_common.Instance) S.Context.t
    | `Step_filer_instance     of (module Common_step_filer.Instance) S.Context.t
    | `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t
    ] )
  S.t
(** A workflow to open a node. Return some data when the node can open *)

val up_directory :
  Up_directory.input ->
  ( (event list, Up_directory.error) result,
    [> `Step_common_instance    of (module Common_step_common.Instance) S.Context.t
    | `Step_filer_instance     of (module Common_step_filer.Instance) S.Context.t
    | `Step_file_list_instance of (module Common_step_file_list.Instance) S.Context.t
    ] )
  S.t
(** A workflow to up directory of specified side. Don't do anything if the side is already located root directory *)

val toggle_mark :
  Toggle_mark.input ->
  ( (event list, Toggle_mark.error) result,
    [> `Step_filer_instance of (module Common_step_filer.Instance) S.Context.t ] )
  S.t
(** A workflow to toggle mark of the item *)
