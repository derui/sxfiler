module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module M = Modules
module N = Jsoo_node

exception No_main_window

type t = {
  mutable main_window: FFI.BrowserWindow.t Js.t option;
  ipc: FFI.ipc Js.t;
  fs: N.Module_types.fs Js.t;
  runner: Flux_runner.t;
}

let on_ready t _ =
  let main_window = M.browser_window @@ object%js
      val height = 600
      val width = 800
      val resizable = Js.bool true
      val acceptFirstMouse = Js.bool true

      val webPreferences = Js.Optdef.empty
    end
  in
  main_window##.webContents##openDevTools ();
  t.main_window <- Some main_window;

  let file_path = N.Path.join [Js.to_string N.__dirname;"index.html"] in

  match t.main_window with
  | None -> raise No_main_window
  | Some window -> begin
      window##loadURL Js.(string ("file://" ^ file_path));
      window##focusOnWebView ()
    end

let make ~ipc ~fs ~runner = {
  main_window = None;
  ipc;
  fs;
  runner;
}
