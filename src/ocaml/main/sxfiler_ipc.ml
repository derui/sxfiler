
module E = Sxfiler_common.Std.Event
module FFI = Sxfiler_common.Std.Ffi
module M = Sxfiler_common.Std.Message
module P = Sxfiler_main_process

exception Unhandled_promise

(**
 * IPC event management for Main process.
 *
 * This module provide only event handlers for request event in {IPC_KEYS}.
 * User must not instance greater than two times, so user only initialize in Main class
 * for Main process.
*)
let on_request_files_in_directory t ev path =
  let module File_list = Sxfiler_file_list in
  let module M = Sxfiler_modules in
  let path_ = M.path in
  let path = Js.string path in
  let absolute = path_##resolve Js.(array [|path|]) |> Js.to_string in
  let open Lwt.Infix in
  let lwt = File_list.get_file_stats ~fs:t.P.fs absolute
    >>= (fun files ->
        let module M = Sxfiler_common.Std.Message in
        let module R = Sxfiler_flux_runner in
        Lwt.return @@ R.send t.P.runner (M.finish_files_in_directory (None, absolute, files))
      )
  in

  let lwt = Lwt.catch (fun () -> lwt) (fun err ->
      Firebug.console##log err;
      match err with
      | File_list.Not_directory f ->
        let module M = Sxfiler_common.Std.Message in
        let module R = Sxfiler_flux_runner in
        Lwt.return @@ R.send t.P.runner (M.finish_files_in_directory (Some err, absolute, [||]))
      | _ -> raise Unhandled_promise
    )
  in
  Lwt.ignore_result lwt

let on_action t ev = function
  | M.REQUEST_FILES_IN_DIRECTORY v -> on_request_files_in_directory t ev v
  | M.REQUEST_QUIT_APPLICATION -> P.on_quit t ev
  | _ -> ()

let bind t =
  let listener ev = function
    | `Action v -> on_action t ev @@ E.M.of_js v
    | _ -> ()
  in
  E.IPC.(on ~target:action ~f:listener t.P.ipc)
