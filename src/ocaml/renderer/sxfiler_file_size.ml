module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method size: Js.number Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let size = Js.float_of_number props##.size |> int_of_float in
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className = Sxfiler_classnames.make ["fp-FileList_FileSize"]
      end)
      ~children:[|
        R.text @@ string_of_int size
      |]
  )
