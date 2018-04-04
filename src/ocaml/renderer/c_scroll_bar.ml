module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method start: float Js.readonly_prop
      method windowSize: float Js.readonly_prop
    end
  end)


let visible_window key start size =
  R.Dom.of_tag `span
    ~key ~props:R.(element_spec () ~class_name:"fp-ScrollBar_VisibleWindow"
                     ~others:(object%js
                       val style = object%js
                         val top = Printf.sprintf "%f%%" (start *. 100.0)
                         val height = Printf.sprintf "%f%%" (size *. 100.0)
                       end
                     end))

let component = Component.make (fun props ->
    R.Dom.of_tag `div
      ~props:R.(element_spec ~class_name:"fp-ScrollBar" ())
      ~children:[visible_window "bar" props##.start props##.windowSize]
  )
