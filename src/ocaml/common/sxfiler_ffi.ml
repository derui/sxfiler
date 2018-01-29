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
    method dev: number t readonly_prop
    method ino: number t readonly_prop
    method mode: number t readonly_prop
    method unlink: number t readonly_prop
    method uid: number t readonly_prop
    method gid: number t readonly_prop
    method rdev: number t readonly_prop
    method size: number t readonly_prop
    method blksize: number t readonly_prop
    method blocks: number t readonly_prop
    method atime: date t readonly_prop
    method mtime: date t readonly_prop
    method ctime: date t readonly_prop
    method birthtime: date t readonly_prop
    method isDirectory: bool t meth
    method isFile: bool t meth
    method isBlockDevice: bool t meth
    method isSymbolicLink: bool t meth
  end

  class type stat_obj = object
    method dev: number t readonly_prop
    method ino: number t readonly_prop
    method mode: number t readonly_prop
    method unlink: number t readonly_prop
    method uid: number t readonly_prop
    method gid: number t readonly_prop
    method rdev: number t readonly_prop
    method size: number t readonly_prop
    method blksize: number t readonly_prop
    method blocks: number t readonly_prop
    method atime: float readonly_prop
    method mtime: float readonly_prop
    method ctime: float readonly_prop
    method birthtime: float readonly_prop
    method isDirectory: bool t readonly_prop
    method isFile: bool t readonly_prop
    method isBlockDevice: bool t readonly_prop
    method isSymbolicLink: bool t readonly_prop
  end

  (* Convert Stats class to pure json *)
  let stat_to_obj t = object%js
    val dev = t##.dev
    val ino = t##.ino
    val mode = t##.mode
    val unlink = t##.unlink
    val uid = t##.uid
    val gid = t##.gid
    val rdev = t##.rdev
    val size = t##.size
    val blksize = t##.blksize
    val blocks = t##.blocks
    val atime = t##.atime##getTime
    val mtime = t##.mtime##getTime
    val ctime = t##.ctime##getTime
    val birthtime = t##.birthtime##getTime
    val isDirectory = t##isDirectory
    val isFile = t##isFile
    val isBlockDevice = t##isBlockDevice
    val isSymbolicLink = t##isSymbolicLink
  end

  class type t = object
    method statSync: js_string Js.t -> stat Js.t meth
    method lstatSync: js_string Js.t -> stat Js.t meth
    method readlinkSync: js_string Js.t -> js_string Js.t meth
    method readdirSync: js_string Js.t -> js_string Js.t js_array Js.t meth
    method readFileSync: js_string Js.t -> js_string Js.t Js.t meth
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
  method on: js_string Js.t -> 'a  callback -> unit meth
  method once: js_string Js.t -> 'a callback -> unit meth
  method send: js_string t -> 'a -> unit meth
  method removeAllListener: js_string Js.t -> unit meth
  method removeAllListener_all: unit -> unit meth
end

class type ipc = object
  (* handle some event raised from EventEmitter *)
  method on: js_string Js.t -> (Event.t Js.t -> 'a -> unit) callback -> unit meth
  method once: js_string Js.t -> (Event.t Js.t -> 'a -> unit) callback -> unit meth
  method send: js_string t -> 'a -> unit meth
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
    method webContents_ipc: ipc Js.t readonly_prop
  end
end

class type crash_reporter = object
  method start: unit -> unit meth
end

class type electron_app = object
  inherit event_emitter

  method quit: unit -> unit meth
  method getPath: js_string t -> js_string t meth
  method getAppPath: js_string t meth
end

class type electron = object
  method ipcMain: ipc t readonly_prop
  method ipcRenderer: ipc t readonly_prop
  method app: electron_app t readonly_prop
end
