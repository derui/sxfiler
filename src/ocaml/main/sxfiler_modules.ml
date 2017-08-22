module FFI = Sxfiler_common.Std.Ffi

(** Call require and return module. This function should use for nodejs's module. *)
let require module_ : 'a Js.t =
  let require = Js.Unsafe.pure_js_expr "require" in
  let module_ = Js.string module_ in
  Js.Unsafe.(fun_call require [|inject module_|])

let path : FFI.path Js.t =
  let original = require "path" in
  object%js
    method resolve = fun v ->
      let v = Js.to_array v |> Array.map Js.Unsafe.inject in
      Js.Unsafe.fun_call (Js.Unsafe.get original "resolve") v

    method join = fun v ->
      let v = Js.to_array v |> Array.map Js.Unsafe.inject in
      Js.Unsafe.fun_call (Js.Unsafe.get original "join") v
  end
let fs : FFI.Fs.t Js.t = require "fs"
let original_fs : FFI.Fs.t Js.t = require "original-fs"

let electron = require "electron"

let crash_reporter : FFI.crash_reporter Js.t =
  Js.Unsafe.get electron "crashReporter" 

let browser_window: FFI.BrowserWindow.option Js.t -> FFI.BrowserWindow.t Js.t = fun option ->
  let cls = Js.Unsafe.get electron "BrowserWindow" in
  new%js cls option
