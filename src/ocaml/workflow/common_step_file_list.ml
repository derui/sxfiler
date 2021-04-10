open Abbrev
include Common_step_intf.File_list

(* implementations *)

let scan t =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_file_list_instance c) in
  let module I = (val instance : Instance) in
  let location = t.D.File_list.location in
  S.return_lwt
  @@ match%lwt I.scan_location location with
     | Error (`Not_exists _)    -> Lwt.return (D.File_list.scan `No_location t)
     | Error (`Not_directory _) -> D.File_list.scan `No_location t |> Lwt.return
     | Ok items                 -> D.File_list.scan (`Scanned items) t |> Lwt.return

let reload t =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_file_list_instance c) in
  let module I = (val instance : Instance) in
  let location =
    match t with D.File_list.Valid { location; _ } -> location | No_location { location; _ } -> location
  in
  S.return_lwt
  @@ match%lwt I.scan_location location with
     | Error (`Not_exists _)    -> D.File_list.reload `No_location t |> Lwt.return
     | Error (`Not_directory _) -> D.File_list.reload `No_location t |> Lwt.return
     | Ok items                 ->
         let ids = D.File_list.marked_items t |> List.map D.File_item.id in
         D.File_list.reload (`Scanned items) t |> D.File_list.mark_items ~ids |> Lwt.return
