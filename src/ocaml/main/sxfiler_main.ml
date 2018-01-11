module E = Sxfiler_common.Std.Event
module FFI = Sxfiler_common.Std.Ffi
module Main_ipc = Sxfiler_ipc
module Main_process = Sxfiler_main_process
module M = Sxfiler_modules

let dirname : Js.js_string Js.t option = Js.Unsafe.global##.__dirname

let () =
  (* M.crash_reporter##start () |> ignore; *)

  let ipc = M.electron##.ipcMain in
  let main_ipc = Main_ipc.Core.make ~ipc ~fs:(M.original_fs) in
  let main_process = Main_process.make main_ipc in

  E.IPC.(ipc |> on ~target:Listener.request_quit_application ~f:(fun _ _ -> Main_process.on_quit main_process ()));
  let app : FFI.electron_app Js.t = M.electron##.app in begin
    let channel = Js.string "ready" in
    let listener = Js.wrap_callback (fun _ _ -> Main_process.on_ready main_process ()) in
    app##on channel listener;

    let channel = Js.string "window-all-closed" in
    let listener = Js.wrap_callback (fun () -> app##quit ()) in
    app##on channel listener
  end
