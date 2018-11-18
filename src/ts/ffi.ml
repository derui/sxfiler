(* This module defines Bucklescript's FFI for some nodejs modules. *)
open Js

(* A module binding for Event structure is sended from IPC *)
module Event = struct
  class type sender =
    object
      method send : js_string Js.t -> 'a Opt.t -> unit meth
    end

  class type t =
    object
      method sender : sender Js.t readonly_prop
    end
end

class type ipc =
  object
    (* handle some event raised from EventEmitter *)
    method on : js_string Js.t -> (Event.t Js.t -> 'a -> unit) callback -> unit meth

    method on_0 : js_string Js.t -> (Event.t Js.t -> unit) callback -> unit meth

    method once : js_string Js.t -> (Event.t Js.t -> 'a -> unit) callback -> unit meth

    method once_0 : js_string Js.t -> (Event.t Js.t -> unit) callback -> unit meth

    method send : js_string t -> 'a -> unit meth

    method removeAllListener : js_string Js.t -> unit meth

    method removeAllListener_all : unit -> unit meth
  end

module BrowserWindow = struct
  class type web_preferences =
    object
      method devTools : bool Js.t readonly_prop
    end

  class type option =
    object
      method width : int readonly_prop

      method height : int readonly_prop

      method resizable : bool Js.t readonly_prop

      method acceptFirstMouse : bool Js.t readonly_prop

      method webPreferences : web_preferences Js.t Js.optdef readonly_prop
    end

  class type web_contents =
    object
      method openDevTools : unit -> unit meth
    end

  class type t =
    object
      method loadURL : js_string Js.t -> unit meth

      method focusOnWebView : unit -> unit meth

      method close : unit -> unit meth

      method webContents : web_contents Js.t readonly_prop

      method webContents_ipc : ipc Js.t readonly_prop
    end

  module Web_contents_event = struct
    let on_did_finish_load ~(browser_window : #t Js.t) ~listener =
      let channel = Js.string "did-finish-load" in
      let ipc = browser_window##.webContents_ipc in
      ipc##on_0 channel (Js.wrap_callback listener)
  end
end

module Crash_reporter = struct
  class type option =
    object
      method companyName : js_string t optdef readonly_prop

      method submitURL : js_string t readonly_prop

      method uploadToServer : bool t optdef readonly_prop

      method crashesDirectory : js_string t optdef readonly_prop
    end

  class type t =
    object
      method start : option Js.t optdef -> unit meth
    end
end

class type electron_app =
  object
    inherit Jsoo_node.Events.event_emitter

    method quit : unit -> unit meth

    method getPath : js_string t -> js_string t meth

    method getAppPath : js_string t meth
  end

class type electron =
  object
    method ipcMain : ipc t readonly_prop

    method ipcRenderer : ipc t readonly_prop

    method app : electron_app t readonly_prop
  end
