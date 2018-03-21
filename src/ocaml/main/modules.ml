module N = Jsoo_node
module FFI = Sxfiler_common.Ffi

(** Call require and return module. This function should use for nodejs's module. *)
let require module_ : 'a Js.t =
  let require = Js.Unsafe.pure_js_expr "require" in
  let module_ = Js.string module_ in
  Js.Unsafe.(fun_call require [|inject module_|])

let fs : unit -> N.Module_types.fs Js.t = fun () -> require "fs"
let original_fs : unit -> N.Module_types.fs Js.t = fun () -> require "original-fs"

let electron () = require "electron"

let crash_reporter : unit -> FFI.Crash_reporter.t Js.t = fun () ->
  let electron = electron () in
  Js.Unsafe.get electron "crashReporter"

let browser_window: FFI.BrowserWindow.option Js.t -> FFI.BrowserWindow.t Js.t = fun option ->
  let electron = electron () in
  let cls = Js.Unsafe.get electron "BrowserWindow" in
  new%js cls option
