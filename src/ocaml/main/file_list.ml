module T = Sxfiler_common.Types
module Path = Jsoo_node.Path
module Fs = Jsoo_node.Fs

exception Not_directory of string

(**
 * Get files in the directory.
 *
 * Caution, this method needs the fs module of execution environment, such as fs on node or
 * original-fs on electron.
 *)
let get_file_stats ~fs path =
  let open Lwt.Infix in
  let filename_to_stats names =
    let names = Array.map (fun v -> Path.join [path; v]) names |> Array.to_list in
    let linked_to filename stat =
      if Js.to_bool stat##.isSymbolicLink then begin
          match Fs.readlinkSync filename with
          | Ok link_path -> Some link_path
          | _ -> None
        end
      else None
    in
    let get_file_stat filename =
      match Fs.lstatSync filename with
      | Ok stat -> begin
          let stat = Fs.stat_to_obj stat in
          let link_path = linked_to filename stat in
          let directory = Path.dirname filename in
          let filename = Path.basename filename in
          Some (T.File_stat.make ~filename ~stat ~link_path ~directory)
        end
      | Error _ -> None
    in
    List.map get_file_stat names
    |> List.filter Sxfiler_common.Util.Option.is_some
    |> List.map (Sxfiler_common.Util.Option.get_exn)
  in

  Lwt_js.yield ()
  >>= Lwt.wrap1 (fun () -> Fs.statSync path)
  >>= (function
       | Ok stat when Js.to_bool stat##isDirectory -> Lwt.return @@ Fs.readdirSync path
       | Ok stat when Js.to_bool stat##isDirectory |> not -> Lwt.fail (Not_directory path)
       | Error e -> Lwt.fail e)
  >>= (function
       | Ok names -> Lwt.return @@ filename_to_stats names
       | Error e -> Lwt.fail e)
