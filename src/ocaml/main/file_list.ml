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
  let current = Lwt_js.yield ()
    >>= fun () -> let stat = Fs.statSync path in Lwt.return stat
  in

  current
  >>= (fun stat ->
      match stat with
      | Ok stat -> begin
          if not (Js.to_bool stat##isDirectory) then
            Lwt.fail (Not_directory path)
          else
            Lwt.return @@ Fs.readdirSync path
        end
      | Error e -> Lwt.fail e
    )
  >>= (function
      | Ok names -> begin
          let names = Array.map (fun v -> Path.join [path; v]) names |> Array.to_list in
          let names = List.map (fun filename ->
              match Fs.lstatSync filename with
              | Ok stat -> begin
                  let stat = Fs.stat_to_obj stat in
                  let link_path = match (stat##.isSymbolicLink |> Js.to_bool) with
                    | false -> None
                    | true -> begin
                        match Fs.readlinkSync filename with
                        | Ok link_path -> Some link_path
                        | _ -> None
                      end
                  in
                  Some (T.File_stat.make ~filename ~stat ~link_path)
                end
              | Error _ -> None
            ) names
                      |> List.filter (function
                          | Some _ -> true
                          | None -> false
                        )
                      |> List.map (Sxfiler_common.Util.Option.get_exn)
          in
          Lwt.return names
        end
      | Error e -> Lwt.fail e
    )
