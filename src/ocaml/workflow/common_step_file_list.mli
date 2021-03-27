open Abbrev

include module type of Common_step_intf.File_list

val scan :
  D.File_list.unscanned -> (D.File_list.scanned, [> `Step_file_list_instance of (module Instance) S.Context.t ]) S.t
(** [scan location_exists scan_location t] returns updated [t] with file items scanned by [scan_location]. *)

val reload :
  D.File_list.scanned -> (D.File_list.scanned, [> `Step_file_list_instance of (module Instance) S.Context.t ]) S.t
(** [reload scan_location t] reload files from same location that [t] has. *)
