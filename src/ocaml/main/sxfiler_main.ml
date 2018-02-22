module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module M = Modules
module T = Sxfiler_common.Types

module C = Config_loader

let dirname : Js.js_string Js.t option = Js.Unsafe.global##.__dirname

let argv =
  let argv = Js.to_array Js.Unsafe.global##.process##.argv in
  Array.sub argv 2 (Array.length argv - 2)

let option_config = ref ""
let options = [
  ("--config", Arg.Set_string option_config, "Path for configuraiton")
]

let subscription ipc t =
  let module S = Sxfiler_common.State in
  Lwt.return @@ E.IPC.(send ~channel:(update (S.to_js t)) ~ipc)

let make_initial_pane id = T.Pane.make ~id ~directory:"." ()

let () =
  M.crash_reporter##start (Js.Optdef.return @@ object%js
                             val companyName = Js.string "sxfiler" |> Js.Optdef.return
                             val submitURL = Js.string ""
                             val uploadToServer = Js.Optdef.return @@ Js.bool false
                             val crashesDirectory = Js.Optdef.empty
                           end);
  let app : FFI.electron_app Js.t = M.electron##.app in
  Arg.parse_argv argv options ignore "Sxfiler";

  let config = C.load Js.(to_string app##getAppPath) !option_config in
  let initial_state = Sxfiler_common.State.{empty with config} in
  let runner = Flux_runner.run ~initial_state () in
  let ipc = M.electron##.ipcMain in
  let window_binder = Window_binder.make ~ipc ~fs:(M.original_fs) ~runner in
  Ipc_handler.bind window_binder;

  let module Subscription = struct
    let handle t =
      match window_binder.Window_binder.main_window with
      | None -> Lwt.return_unit
      | Some w -> begin
          let module S = Sxfiler_common.State in
          if t.S.terminated then app##quit () |> Lwt.return
          else subscription w##.webContents_ipc t
        end
  end in
  Flux_runner.subscribe runner ~subscription:(module Subscription);

  begin
    let channel = Js.string "ready" in
    let listener = Js.wrap_callback (fun _ _ ->
        Window_binder.on_ready window_binder ();

        match window_binder.Window_binder.main_window with
        | None -> ()
        | Some bw ->
          let module M = Sxfiler_common.Message in
          let module E = FFI.BrowserWindow.Web_contents_event in
          E.on_did_finish_load ~browser_window:bw
            ~listener:(fun _ ->
                let module S = Sxfiler_common.State in
                List.iter (fun pane ->
                    let module P = Sxfiler_common.Types.Pane in
                    let pane = P.to_js pane
                    and dir = Js.string pane.P.directory in
                    let message = M.request_files_in_directory (pane, dir) in
                    Flux_runner.send runner message
                  ) [initial_state.S.left_pane; initial_state.S.right_pane]
              )
      ) in
    app##on channel listener;

    let channel = Js.string "window-all-closed" in
    let listener = Js.wrap_callback (fun () -> app##quit ()) in
    app##on channel listener
  end
