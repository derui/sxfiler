include module type of Common_step_intf.File_list

val scan : scan
(** [scan location_exists scan_location t] returns updated [t] with file items scanned by [scan_location]. *)

val reload : reload
(** [reload scan_location t] reload files from same location that [t] has. *)
