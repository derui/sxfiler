module FFI = Sxfiler_common.Std.Ffi
module T = Sxfiler_common.Std.Types

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
      if not (stat##isDirectory () |> Js.to_bool) then
        Lwt.fail (Not_directory path)
      else
        Lwt.return @@ fs##readdirSync path_)
  >>= (fun names ->
      let module M = Sxfiler_modules in
      let names = Js.to_array names
                  |> Array.map (fun v -> M.path##join (Js.array [|path_; v|])) in
      Lwt.return @@
      Array.map (fun name ->
          let stat = fs##statSync name in
          let stat = FFI.Fs.stat_to_obj stat in
          {T.File_stat.filename = Js.to_string name; stat}
        ) names
    )
