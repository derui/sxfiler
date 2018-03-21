module N = Jsoo_node
module FFI = Sxfiler_common.Ffi

(** Call require and return module. This function should use for nodejs's module. *)
let require module_ : 'a Js.t =
  let require = Js.Unsafe.pure_js_expr "require" in
  let module_ = Js.string module_ in
  Js.Unsafe.(fun_call require [|inject module_|])

let fs () : N.Module_types.fs Js.t = require "fs"
let original_fs () : N.Module_types.fs Js.t = require "original-fs"

let electron () = require "electron"

let crash_reporter () : FFI.Crash_reporter.t Js.t =
  Js.Unsafe.get electron "crashReporter"

let browser_window: FFI.BrowserWindow.option Js.t -> FFI.BrowserWindow.t Js.t = fun option ->
  let cls = Js.Unsafe.get electron "BrowserWindow" in
  new%js cls option
