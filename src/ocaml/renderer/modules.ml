let electron : Ffi.electron Js.t = Js.Unsafe.pure_js_expr "require('electron')"

module Lodash = struct
  (** wrapper script for lodash.isequal module *)
  let isEqual : 'a Js.t -> 'b Js.t -> bool =
    fun o1 o2 ->
      let f = Js.Unsafe.pure_js_expr "require('lodash.isequal')" in
      Js.Unsafe.fun_call f Js.Unsafe.[|inject o1; inject o2|] |> Js.to_bool
end
