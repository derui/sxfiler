module C = Sxfiler_common.Std
module R = Reactjscaml.Std

module Component = R.Component.Make_stateless (struct
    class type _t = object
      method timestamp: float Js.readonly_prop
    end
    type t = _t Js.t
  end)

let format_date date =
  Printf.sprintf "%04d/%02d/%02d %02d:%02d:%02d"
    (date##getFullYear)
    (date##getMonth + 1)
    (date##getDate)
    (date##getHours)
    (date##getMinutes)
    (date##getSeconds)

let component = Component.make (fun props ->
    let timestamp = props##.timestamp in
    let date = new%js Js.date_fromTimeValue timestamp in
    let date = format_date date in
    R.Dom.of_tag `span
      ~props:(object%js
        val key = Js.Optdef.empty
        val className =
          let open Sxfiler_classnames.Infix in
          Sxfiler_classnames.(["file-list__file-timestamp"]
                              <|> Style.Grid.item_row 1
                              <|> Style.Grid.item_col 3)
          |> Sxfiler_classnames.make
      end)
      ~children:[| R.text date |]
  )
