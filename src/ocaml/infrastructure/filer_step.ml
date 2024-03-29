open Sxfiler_core
open Sxfiler_workflow
module D = Sxfiler_domain

module Log = (val Logger.make [ "infra"; "filer_step" ])

(* implementations with infrastructure *)

module type State = Statable.S with type state = Sxfiler_domain.Filer.t option

module Instance (S : State) = struct
  let get () = S.get ()

  let delete_item item =
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

  let copy_item { Common_step.Filer.source; dest; overwrite } =
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
               | _          -> Unknown (Unix.error_message e))
           | _ as e                    -> Unknown (Printexc.to_string e)))

  let move_item { Common_step.Filer.source; dest; overwrite } =
    let source' = Path.to_string source in
    let dest' = Path.to_string dest in
    let move dst =
      Lwt_unix.rename source' dst |> Lwt_result.catch
      |> Lwt_result.map_err (function
           | Unix.Unix_error (e, _, _) -> (
               match e with
               | Unix.EPERM -> Common_step.Filer.No_permission "can not move"
               | _          -> Common_step.Filer.Unknown (Unix.error_message e))
           | _ as e                    -> Common_step.Filer.Unknown (Printexc.to_string e))
    in
    if Sys.file_exists dest' && not & overwrite then Lwt.return_error (Common_step.Filer.Destination_exists dest)
    else if not & Sys.file_exists & Path.dirname dest then Lwt.return_error (Common_step.Filer.Not_exists dest)
    else
      let%lwt () = Log.debug (fun m -> m "Move file: [%s] -> [%s]" source' dest') in
      move dest'
end
