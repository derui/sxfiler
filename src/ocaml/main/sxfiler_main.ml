module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module Main_ipc = Sxfiler_ipc
module Main_process = Sxfiler_main_process
module M = Sxfiler_modules

module H = Sxfiler_key_map
module C = Sxfiler_config

let subscription ipc t =
  let module S = Sxfiler_common.State in
  Lwt.return @@ E.IPC.(send ~channel:(`Update (S.to_js t)) ~ipc)

let dirname : Js.js_string Js.t option = Js.Unsafe.global##.__dirname

let argv =
  let argv = Js.to_array Js.Unsafe.global##.process##.argv in
  Array.sub argv 2 (Array.length argv - 2)

let option_config = ref ""
let options = [
  ("--config", Arg.Set_string option_config, "Path for configuraiton")
]

let () =
  let app : FFI.electron_app Js.t = M.electron##.app in
  Arg.parse_argv argv options ignore "Sxfiler";

  let config = C.load app##getAppPath !option_config in
  let runner = Sxfiler_flux_runner.run ~initial_state:(Sxfiler_common.State.empty ()) () in
  let ipc = M.electron##.ipcMain in
  let main_process = Main_process.make ~ipc ~fs:(M.original_fs) ~runner ~key_map:config.C.key_map in
  Main_ipc.bind main_process;

  let module Subscription = struct
    let handle t =
      match main_process.Main_process.main_window with
      | None -> Lwt.return_unit
      | Some w -> subscription w##.webContents_ipc t
  end in
  Sxfiler_flux_runner.subscribe runner ~subscription:(module Subscription);

  begin
    let channel = Js.string "ready" in
    let listener = Js.wrap_callback (fun _ _ ->
        Main_process.on_ready main_process ();
        let module M = Sxfiler_common.Message in
        Sxfiler_flux_runner.send runner (M.request_files_in_directory ".")
      ) in
    app##on channel listener;

    let channel = Js.string "window-all-closed" in
    let listener = Js.wrap_callback (fun () -> app##quit ()) in
    app##on channel listener
  end
