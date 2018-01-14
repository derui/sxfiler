module E = Sxfiler_common.Std.Event
module FFI = Sxfiler_common.Std.Ffi
module Main_ipc = Sxfiler_ipc
module Main_process = Sxfiler_main_process
module M = Sxfiler_modules

let subscription ipc t =
  let module S = Sxfiler_common.Std.State in
  Lwt.return @@ E.IPC.(send ~channel:(`Update (S.to_js t)) ~ipc)

let dirname : Js.js_string Js.t option = Js.Unsafe.global##.__dirname

let () =
  (* M.crash_reporter##start () |> ignore; *)

  let runner = Sxfiler_flux_runner.run ~initial_state:(Sxfiler_common.Std.State.empty ()) () in
  let ipc = M.electron##.ipcMain in
  let main_process = Main_process.make ~ipc ~fs:(M.original_fs) ~runner in
  Main_ipc.bind main_process;

  let module Subscription = struct
    let handle t =
      match main_process.Main_process.main_window with
      | None -> Lwt.return_unit
      | Some w -> subscription w##.webContents_ipc t
  end
  in
  Sxfiler_flux_runner.subscribe runner ~subscription:(module Subscription);

  let app : FFI.electron_app Js.t = M.electron##.app in
  begin
    let channel = Js.string "ready" in
    let listener = Js.wrap_callback (fun _ _ -> Main_process.on_ready main_process ()) in
    app##on channel listener;

    let channel = Js.string "window-all-closed" in
    let listener = Js.wrap_callback (fun () -> app##quit ()) in
    app##on channel listener
  end
