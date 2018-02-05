module FFI = Sxfiler_common.Ffi
module T = Sxfiler_common.Types

exception Not_directory of string

(**
 * Get files in the directory.
 *
 * Caution, this method needs the fs module of execution environment, such as fs on node or
 * original-fs on electron.
*)
let get_file_stats ~fs path =
  let path_ = Js.string path in
  let open Lwt.Infix in
  let current = Lwt_js.yield ()
    >>= fun () ->
    let stat = fs##statSync path_ in Lwt.return stat
  in

  current
  >>= (fun stat ->
      if not (Js.to_bool stat##isDirectory) then
        Lwt.fail (Not_directory path)
      else
        Lwt.return @@ fs##readdirSync path_)
  >>= (fun names ->
      let names = Js.to_array names
                  |> Array.map (fun v -> Modules.path##join (Js.array [|path_; v|])) in
      Lwt.return @@
      Array.map (fun name ->
          let stat = fs##lstatSync name |> FFI.Fs.stat_to_obj in
          let link_path = match (stat##.isSymbolicLink |> Js.to_bool) with
            | false -> None
            | true -> let link_path = fs##readlinkSync name in
              Some (Js.to_string link_path)
          in
          T.File_stat.make ~filename:(Js.to_string name) ~stat ~link_path
        ) names
    )
