
module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module M = Sxfiler_common.Message
module P = Sxfiler_main_process

exception Unhandled_promise

(** Handle request_files_in_directory message *)
let on_request_files_in_directory t path =
  let module File_list = Sxfiler_file_list in
  let module M = Sxfiler_modules in
  let path_ = M.path in
  let path = Js.string path in
  let absolute = path_##resolve Js.(array [|path|]) |> Js.to_string in

  let open Lwt.Infix in
  let lwt = File_list.get_file_stats ~fs:t.P.fs absolute
    >>= (fun files ->
        let module M = Sxfiler_common.Message in
        let module R = Sxfiler_flux_runner in
        Lwt.return @@ R.send t.P.runner (M.finish_files_in_directory (None, absolute, files))
      )
  in

  let lwt = Lwt.catch (fun () -> lwt) (fun err ->
      Firebug.console##log err;
      match err with
      | File_list.Not_directory f ->
        let module M = Sxfiler_common.Message in
        let module R = Sxfiler_flux_runner in
        Lwt.return @@ R.send t.P.runner (M.finish_files_in_directory (Some err, absolute, [||]))
      | _ -> raise Unhandled_promise
    )
  in
  Lwt.ignore_result lwt

let on_quit t =
  match t.P.main_window with
  | None -> raise P.No_main_window
  | Some window -> window##close ()

let handle_action t = function
  | M.REQUEST_FILES_IN_DIRECTORY v -> on_request_files_in_directory t v
  | M.REQUEST_QUIT_APPLICATION -> on_quit t
  | _ -> ()

let send_message_to_store t action =
  let module R = Sxfiler_flux_runner in
  R.send t.P.runner action

let on_action t action = List.iter (fun f -> f t action) [handle_action;send_message_to_store]
