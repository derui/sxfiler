open Sxfiler_core
open Abbrev

type scan_location_error =
  [ `Not_exists    of Path.t
  | `Not_directory of Path.t
  ]

type scan_location = Path.t -> (D.File_item.t list, scan_location_error) result Lwt.t

type scan = scan_location -> D.File_list.unscanned -> D.File_list.scanned Lwt.t
(** workflow type to move location of a file_list [t] *)

type reload = scan_location -> D.File_list.scanned -> D.File_list.scanned Lwt.t
(** workflow to reload a file_list [t] *)

(* implementations *)

let scan : scan =
 fun scan_location t ->
  let location = t.location in
  match%lwt scan_location location with
  | Error (`Not_exists _)    -> Lwt.return (D.File_list.scan `No_location t)
  | Error (`Not_directory _) -> D.File_list.scan `No_location t |> Lwt.return
  | Ok items                 -> D.File_list.scan (`Scanned items) t |> Lwt.return

let reload : reload =
 fun scan_location t ->
  let location = match t with Valid { location; _ } -> location | No_location { location; _ } -> location in
  match%lwt scan_location location with
  | Error (`Not_exists _)    -> D.File_list.reload `No_location t |> Lwt.return
  | Error (`Not_directory _) -> D.File_list.reload `No_location t |> Lwt.return
  | Ok items                 ->
      let ids = D.File_list.marked_items t |> List.map D.File_item.id in
      D.File_list.reload (`Scanned items) t |> D.File_list.mark_items ~ids |> Lwt.return
