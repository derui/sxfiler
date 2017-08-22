module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method mode: Js.number Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let mode = Js.float_of_number props##.mode in
    let mode = int_of_float mode in
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className = Sxfiler_classnames.make ["file-list__file-mode"]
      end)
      ~children:[|
        R.text @@ string_of_int mode
      |]
  )
