module FFI = Sxfiler_common.Std.Ffi

(** Call require and return module. This function should use for nodejs's module. *)
let require module_ : 'a Js.t =
  Js.Unsafe.pure_js_expr @@ Printf.sprintf "require(%s)" module_

let electron = require "electron"
