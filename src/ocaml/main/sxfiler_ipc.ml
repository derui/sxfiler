
module E = Sxfiler_common.Std.Event
module FFI = Sxfiler_common.Std.Ffi
module M = Sxfiler_modules

exception Unhandled_promise

(**
 * IPC event management for Main process.
 *
 * This module provide only event handlers for request event in {IPC_KEYS}.
 * User must not instance greater than two times, so user only initialize in Main class
 * for Main process.
*)
module Core = struct
  type t = {
    ipc: FFI.ipc Js.t;
    fs: FFI.Fs.t Js.t;
  }

  let on_request_files_in_directory t ev path =
    let module File_list = Sxfiler_file_list in
    let path_ = M.path in
    let path = Js.string path in
    let absolute = path_##resolve Js.(array [|path|]) |> Js.to_string in
    let open Lwt.Infix in
    let lwt = File_list.get_file_stats ~fs:t.fs absolute
      >>= (fun files ->
          Lwt.return @@ E.IPC.(reply ~channel:(`FINISH_FILES_IN_DIRECTORY (None, absolute, files)) ~ev)
        )
    in

    let lwt = Lwt.catch (fun () -> lwt) (fun err ->
        match err with
        | File_list.Not_directory f ->
          Lwt.return @@ E.IPC.(reply ~channel:(`FINISH_FILES_IN_DIRECTORY (Some err, absolute, [||])) ~ev)

        | _ -> raise Unhandled_promise
      )
    in
    Lwt.ignore_result lwt

  let make ~ipc ~fs =
    let t = {ipc;fs} in
    let listener ev v = on_request_files_in_directory t ev v in
    E.IPC.(on ~target:Listener.request_files_in_directory ~f:listener ipc);
    t
end
