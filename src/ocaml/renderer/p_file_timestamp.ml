module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method timestamp: float Js.readonly_prop
    end
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
      ~props:R.(element_spec ~class_name:"fp-FileItem_Timestamp" ())
      ~children:[R.text date]
  )