module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method file_name: Js.js_string Js.t Js.readonly_prop
    end
    type t = _t Js.t
  end)

let component = Component.make (fun props ->
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className =
          let open Sxfiler_classnames.Infix in
          Sxfiler_classnames.(["fp-FileItem_FileName"]
                              <|> Style.Grid.item_row 1
                              <|> Style.Grid.item_col 4
                             )
          |> Sxfiler_classnames.make

      end)
      ~children:[|
        R.text @@ Js.to_string props##.file_name
      |]
  )
