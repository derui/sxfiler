(* This module defines Bucklescript's FFI for some nodejs modules. *)
open Js

(* FFI for path module *)
class type path = object
  method resolve: js_string t js_array t -> js_string t meth
  method join: js_string t js_array t -> js_string t meth
end

(* FFI for fs and original-fs module *)
module Fs = struct
  class type stat = object
    method mode: Js.number Js.t Js.readonly_prop
    method isDirectory: unit -> bool t meth
  end

  class type t = object
    method statSync: js_string Js.t -> stat Js.t meth
    method readdirSync: js_string Js.t -> js_string Js.t js_array Js.t meth
  end
end

(* A module binding for Event structure is sended from IPC *)
module Event = struct
  class type sender = object
    method send: js_string Js.t -> 'a Opt.t -> unit meth
  end

  class type t = object
    method sender: sender Js.t readonly_prop
  end
end

class type event_emitter = object
  (* handle some event raised from EventEmitter *)
  method on: js_string Js.t -> ('a -> unit) callback -> unit meth
  method once: js_string Js.t -> ('a -> unit) callback -> unit meth
  method removeAllListener: js_string Js.t -> unit meth
  method removeAllListener_all: unit -> unit meth
end

class type ipc = object
  (* handle some event raised from EventEmitter *)
  method on: js_string Js.t -> (Event.t Js.t -> 'a -> unit) callback -> unit meth
  method once: js_string Js.t -> (Event.t Js.t -> 'a -> unit) callback -> unit meth
  method removeAllListener: js_string Js.t -> unit meth
  method removeAllListener_all: unit -> unit meth
end

module BrowserWindow = struct
  class type web_preferences = object
    method devTools: bool Js.t readonly_prop
  end

  class type option = object
    method width: int readonly_prop
    method height: int readonly_prop
    method resizable: bool Js.t readonly_prop
    method acceptFirstMouse: bool Js.t readonly_prop
    method webPreferences: web_preferences Js.t Js.optdef readonly_prop
  end

  class type web_contents = object
    method openDevTools: unit -> unit meth
  end

  class type t = object
    method loadURL: js_string Js.t -> unit meth
    method focusOnWebView: unit -> unit meth
    method close: unit -> unit meth
    method webContents: web_contents Js.t readonly_prop
  end
end

class type crash_reporter = object
  method start: unit -> unit meth
end

class type electron_app = object
  inherit ipc

  method quit: unit -> unit meth
end

class type electron = object
  method ipcMain: ipc t readonly_prop
  method app: electron_app t readonly_prop
end
