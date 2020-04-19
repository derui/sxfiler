open Sxfiler_core
open Abbrev

type scan_location_error =
  [ `Not_exists    of Path.t
  | `Not_directory of Path.t
  ]

type scan_location = Path.t -> (D.File_item.t list, scan_location_error) result Lwt.t
(** Type of step to scan the location *)

type scan = scan_location -> D.File_list.unscanned -> D.File_list.scanned Lwt.t
(** workflow type to move location of a file_list [t] *)

type reload = scan_location -> D.File_list.scanned -> D.File_list.scanned Lwt.t
(** workflow to reload a file_list [t] *)

val scan : scan
(** [scan location_exists scan_location t] returns updated [t] with file items scanned by [scan_location]. *)

val reload : reload
(** [reload scan_location t] reload files from same location that [t] has. *)
