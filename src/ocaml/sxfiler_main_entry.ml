open Sxfiler_main
module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module M = Modules
module T = Sxfiler_common.Types

module C = Config_loader

module U = User_data

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

let default_pane () =
  let directory = Jsoo_node.Path.resolve ["."] in
  T.Pane.make ~directory ()

let restore_state_from_user_data ~state ~user_data =
  let module PH = Sxfiler_common.Pane_history in
  let left_pane_history = user_data.U.left_pane_history
  and right_pane_history = user_data.U.right_pane_history in
  let restore_pane = function
    | [] -> default_pane ()
    | hist :: _ -> T.Pane.make ~focused_item:hist.PH.History.focused_item
                     ~directory:hist.PH.History.directory ()
  in
  let left_pane =  restore_pane left_pane_history
  and right_pane =  restore_pane right_pane_history in

  let restore_history histories =
    let history = PH.make () in
    List.fold_left (fun history v -> PH.add_history ~history:v history) history histories
  in
  Sxfiler_common.State.({
      state with
      left_pane; right_pane;
      left_pane_history = restore_history left_pane_history;
      right_pane_history = restore_history right_pane_history;
    })

let quit_application state app =
  U.save @@ U.of_state state;
  app##quit ()

let () =
  let crash_reporter = M.crash_reporter () in
  crash_reporter##start (Js.Optdef.return @@ object%js
                           val companyName = Js.string "sxfiler" |> Js.Optdef.return
                           val submitURL = Js.string ""
                           val uploadToServer = Js.Optdef.return @@ Js.bool false
                           val crashesDirectory = Js.Optdef.empty
                         end);
  let electron = M.electron () in
  let app : FFI.electron_app Js.t = electron##.app in
  Arg.parse_argv argv options ignore "Sxfiler";

  let config = C.load Js.(to_string app##getAppPath) !option_config in
  let user_data = U.load () in
  let initial_state = Sxfiler_common.State.{empty with config;
                                                       left_pane = default_pane ();
                                                       right_pane = default_pane ();
                                           } in
  let initial_state = restore_state_from_user_data ~state:initial_state ~user_data in
  let runner = Flux_runner.run ~initial_state () in
  let ipc = electron##.ipcMain in
  let original_fs = M.original_fs () in
  let window_binder = Window_binder.make ~ipc ~fs:original_fs ~runner in
  Ipc_handler.bind window_binder;

  let module Subscription = struct
    let handle t =
      match window_binder.Window_binder.main_window with
      | None -> Lwt.return_unit
      | Some w -> begin
          let module S = Sxfiler_common.State in
          if t.S.terminated then quit_application t app |> Lwt.return
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
                List.iter (fun (pane, loc) ->
                    let module P = Sxfiler_common.Types.Pane in
                    let pane = P.to_js pane
                    and dir = Js.string pane.P.directory
                    and loc = T.Pane_location.to_js loc in
                    let message = M.update_pane_request (pane, dir, loc) in
                    Flux_runner.send runner message
                  ) [(initial_state.S.left_pane, `Left); (initial_state.S.right_pane, `Right)]
              )
      ) in
    app##on channel listener;

    let channel = Js.string "window-all-closed" in
    let listener = Js.wrap_callback (fun () -> app##quit ()) in
    app##on channel listener
  end
