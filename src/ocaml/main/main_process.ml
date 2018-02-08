module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module M = Modules
module K = Key_map

let dirname : Js.js_string Js.t = Js.Unsafe.js_expr "__dirname"

exception No_main_window

type t = {
  mutable main_window: FFI.BrowserWindow.t Js.t option;
  ipc: FFI.ipc Js.t;
  fs: FFI.Fs.t Js.t;
  runner: Flux_runner.t;
  key_map: K.key_map;
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

  let file_path =
    let array = Js.array [|dirname;Js.string "index.html"|] in
    M.path##join array |> Js.to_string in

  match t.main_window with
  | None -> raise No_main_window
  | Some window -> begin
      window##loadURL Js.(string ("file://" ^ file_path));
      window##focusOnWebView ()
    end

let make ~ipc ~fs ~runner ~key_map = {
  main_window = None;
  ipc;
  fs;
  runner;
  key_map;
}