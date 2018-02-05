module FFI = Sxfiler_common.Ffi

let electron : FFI.electron Js.t = Js.Unsafe.pure_js_expr "require('electron')"
