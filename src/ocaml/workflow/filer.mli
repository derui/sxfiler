(** Workflows defined by this module *)

include module type of Filer_intf

val initialize :
  Common_step_filer.get -> Common_step_file_list.scan_location -> Common_step_configuration.load -> Initialize.work_flow
(** The workflow to initialize filer with scanning locations *)

val reload_all : Common_step_file_list.scan_location -> Reload_all.work_flow
(** The workflow to reload file lists in both side of the filer *)

val move_location : Common_step_common.now -> Common_step_file_list.scan_location -> Move_location.work_flow
(** The workflow to move location of file list specified side of the filer *)

val copy :
  Common_step_common.now ->
  Common_step_interaction.demand_decision ->
  Common_step_file_list.scan_location ->
  Common_step_filer.copy_item ->
  Copy.work_flow

val move :
  Common_step_common.now ->
  Common_step_interaction.demand_decision ->
  Common_step_file_list.scan_location ->
  Common_step_filer.move_item ->
  Move.work_flow

val delete :
  Common_step_common.now ->
  Common_step_interaction.demand_decision ->
  Common_step_file_list.scan_location ->
  Common_step_configuration.load ->
  Common_step_filer.delete_item ->
  Delete.work_flow

val open_node : Common_step_file_list.scan_location -> Common_step_common.now -> Open_node.work_flow
(** A workflow to open a node. Return some data when the node can open *)

val up_directory : Common_step_file_list.scan_location -> Common_step_common.now -> Up_directory.work_flow
(** A workflow to up directory of specified side. Don't do anything if the side is already located root directory *)

val toggle_mark : Toggle_mark.work_flow
(** A workflow to toggle mark of the item *)
