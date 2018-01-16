module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method state: C.State.js Js.readonly_prop
      method dispatch: Sxfiler_dispatcher.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    let elements = Js.to_array @@ Js.array_map (fun item ->
        R.element ~props:(object%js
          val item = item
        end)
          Sxfiler_file_item.component
      ) props##.state##.file_list
    in
    R.Dom.of_tag `ul
      ~props:(object%js
        val key = Js.Optdef.empty
        val className =
          let open Sxfiler_classnames.Infix in
          Sxfiler_classnames.(["fp-FileList"]
                              <|> Style.Grid.container) |> Sxfiler_classnames.make
      end)
      ~children:elements
  )
