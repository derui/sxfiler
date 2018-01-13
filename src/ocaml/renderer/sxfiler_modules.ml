module FFI = Sxfiler_common.Std.Ffi

let electron : FFI.electron Js.t = Js.Unsafe.pure_js_expr "require('electron')"
