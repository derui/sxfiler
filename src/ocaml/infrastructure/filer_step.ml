open Sxfiler_core
open Sxfiler_workflow
module D = Sxfiler_domain

module Log = (val Logger.make [ "infra"; "filer_step" ])

(* helper module contains conversion function for Unix module *)
module Helper = struct
  let perm_to_mode perm =
    let owner = perm land 0o700 and group = perm land 0o70 and others = perm land 0o7 in
    let int_to_cap v =
      let readable = v land 0o4 = 0o4 and writable = v land 0o2 = 0o2 and executable = v land 0o1 = 0o1 in
      D.File_stat.Capability.make ~writable ~readable ~executable
    in
    D.File_stat.Mode.make
      ~owner:(int_to_cap (owner lsr 6))
      ~group:(int_to_cap (group lsr 3))
      ~others:(int_to_cap others)

  let stat_to_file_stat path stat =
    let module F = Sxfiler_domain.File_stat in
    let open Result.Infix in
    let* atime = Time.of_float stat.Unix.st_atime |> Option.to_result ~none:"invalid time" in
    let* mtime = Time.of_float stat.Unix.st_mtime |> Option.to_result ~none:"invalid time" in
    let* ctime = Time.of_float stat.Unix.st_mtime |> Option.to_result ~none:"invalid time" in
    let* uid = D.File_stat.Uid.make stat.Unix.st_uid in
    let* gid = D.File_stat.Gid.make stat.Unix.st_gid in
    let* size = Int64.of_int stat.Unix.st_size |> D.File_stat.Size.make in
    let stat' = D.File_stat.Stat.make ~mode:(perm_to_mode stat.Unix.st_perm) ~uid ~gid ~atime ~mtime ~ctime ~size in
    match stat.Unix.st_kind with
    | Unix.S_DIR -> D.File_stat.make_directory stat' |> Result.ok
    | Unix.S_LNK ->
        let* link_path = Unix.readlink path |> Path.of_string |> Result.map_error (fun _ -> "invalid link path") in
        D.File_stat.make_symlink ~stat:stat' ~link_path |> Result.ok
    | _          -> D.File_stat.make_file stat' |> Result.ok
end

(* implementations with infrastructure *)

let scan_location : Common_step.File_list.scan_location =
 fun path ->
  let get_item parent path =
    let path = Filename.concat parent path in
    try
      let open Result.Infix in
      let* stat = Unix.lstat path |> Helper.stat_to_file_stat path in
      let* path' = Path.of_string path |> Result.map_error (fun _ -> "Invalid path") in
      let id = Digest.string path |> Digest.to_hex |> D.File_item.Id.make in
      Ok (D.File_item.make ~id ~full_path:path' ~stat)
    with Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "Error from lstat for %s: %s" path Unix.(error_message e))
  in

  let path' = Path.to_string path in
  if not & Sys.file_exists path' then Lwt.return_error (`Not_exists path)
  else if not & Sys.is_directory path' then Lwt.return_error (`Not_directory path)
  else
    let items = Sys.readdir path' |> Array.to_list in
    let%lwt items = Lwt_list.map_p (Lwt.return % get_item path') items in
    items
    |> List.fold_left
         (fun accum item ->
           match item with
           | Ok item -> item :: accum
           | Error e ->
               Lwt.async (fun () -> Log.warn (fun m -> m "Get error: %s" e));
               accum)
         []
    |> Lwt.return_ok

let delete_item : Common_step.Filer.delete_item =
 fun item ->
  let path = D.File_item.item item |> D.File_item.Item.full_path in
  let path' = path |> Path.to_string in
  if not & Sys.file_exists path' then Lwt.return_error (Common_step.Filer.Not_exists path)
  else
    let%lwt () = File.remove path' |> Lwt.return in
    Log.debug (fun m -> m "Delete item: [%s]" path');%lwt
    Lwt.return_ok ()

module Copy = struct
  module Item_name_set = Set.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

  let buffer_size = 1024

  let copy_file ~source ~dest ~cb =
    let source = Path.to_string source and dest = Path.to_string dest in
    let%lwt ic = Lwt_io.open_file ~flags:[ Unix.O_RDONLY ] ~mode:Lwt_io.Input source in
    let%lwt oc = Lwt_io.open_file ~flags:[ Unix.O_CREAT; Unix.O_WRONLY ] ~mode:Lwt_io.Output dest in
    let rec copy_file' ic oc =
      let%lwt buf = Lwt_io.read ~count:buffer_size ic in
      if String.equal buf "" then Lwt.return_unit
      else
        let%lwt () = Lwt_io.write oc buf in
        copy_file' ic oc
    in
    Lwt_io.with_close_connection (fun (ic, oc) -> copy_file' ic oc) (ic, oc);%lwt
    cb ~source ~dest

  let rec copy_item ~source ~dest ~cb =
    let source' = Path.to_string source in
    let dest' = Path.to_string dest in
    let%lwt source_stat = Lwt_unix.stat source' in
    match source_stat.Unix.st_kind with
    | Unix.S_DIR ->
        let%lwt () = Lwt_unix.mkdir dest' source_stat.Unix.st_perm in
        Lwt_unix.files_of_directory source' |> Lwt_stream.map Filename.basename
        |> Lwt_stream.filter (function "." | ".." -> false | _ -> true)
        |> Lwt_stream.iter_s (fun fname ->
               let source = Path.join source fname in
               let dest = Path.join dest fname in
               copy_item ~source ~dest ~cb)
    | _          -> copy_file ~source ~dest ~cb
end

let copy_item : Common_step.Filer.copy_item =
 fun { source; dest; overwrite } ->
  let on_copied ~source ~dest = Log.debug (fun m -> m "Copy file: [%s] -> [%s]" source dest) in
  let dest' = Path.to_string dest in
  if Sys.file_exists dest' && not overwrite then Lwt.return_error (Common_step.Filer.Destination_exists dest)
  else if not & Sys.file_exists (Path.to_string source) then Lwt.return_error (Common_step.Filer.Not_exists source)
  else if not & Sys.file_exists (Path.dirname dest) then Lwt.return_error (Common_step.Filer.Not_exists dest)
  else (
    Printf.printf "%s\n" (Path.to_string dest);

    Copy.copy_item ~source ~dest ~cb:on_copied
    |> Lwt_result.catch
    |> Lwt_result.map_err (function
         | Unix.Unix_error (e, _, _) -> (
             match e with
             | Unix.EPERM -> Common_step.Filer.No_permission (Unix.error_message e)
             | _          -> Unknown (Unix.error_message e) )
         | _ as e                    -> Unknown (Printexc.to_string e)) )

let move_item : Common_step.Filer.move_item =
 fun { source; dest; overwrite } ->
  let source' = Path.to_string source in
  let dest' = Path.to_string dest in
  let move dst =
    Lwt_unix.rename source' dst |> Lwt_result.catch
    |> Lwt_result.map_err (function
         | Unix.Unix_error (e, _, _) -> (
             match e with
             | Unix.EPERM -> Common_step.Filer.No_permission "can not move"
             | _          -> Common_step.Filer.Unknown (Unix.error_message e) )
         | _ as e                    -> Common_step.Filer.Unknown (Printexc.to_string e))
  in
  if Sys.file_exists dest' && not & overwrite then Lwt.return_error (Common_step.Filer.Destination_exists dest)
  else if not & Sys.file_exists & Path.dirname dest then Lwt.return_error (Common_step.Filer.Not_exists dest)
  else
    let%lwt () = Log.debug (fun m -> m "Move file: [%s] -> [%s]" source' dest') in
    move dest'
