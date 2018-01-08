module FFI = Sxfiler_common.Std.Ffi

(** Call require and return module. This function should use for nodejs's module. *)
let require module_ : 'a Js.t =
  Js.Unsafe.pure_js_expr @@ Printf.sprintf "require(%s)" module_

let electron = require "electron"

let ipc_renderer : FFI.ipc Js.t =
  let prop = Js.string "ipcRenderer" in
  Js.Unsafe.get electron prop
