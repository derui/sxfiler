open Sxfiler_core
module D = Sxfiler_domain

module Log = (val Logger.make [ "infra"; "file_list_step" ])

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

module Instance = struct
  let scan_location path =
    let get_item parent path =
      let path = Filename.concat parent path in
      try
        let open Result.Infix in
        let* stat = Unix.lstat path |> Helper.stat_to_file_stat path in
        let* path' = Path.of_string path |> Result.map_error (fun _ -> "Invalid path") in
        let id = Digest.string path |> Digest.to_hex |> D.File_item.Id.make in
        Ok (D.File_item.make ~id ~full_path:path' ~stat)
      with Unix.Unix_error (e, _, _) ->
        Error (Printf.sprintf "Error from lstat for %s: %s" path Unix.(error_message e))
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
end
