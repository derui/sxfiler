module T = Sxfiler_domain
module R = Jsoo_reactjs

let visible_window key start size =
  [%e
    span ~key ~class_name:"fp-ScrollBar_VisibleWindow"
      ~others:
        (object%js
          val style =
            object%js
              val top = Printf.sprintf "%f%%" (start *. 100.0)

              val height = Printf.sprintf "%f%%" (size *. 100.0)
            end
        end)]

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method start : float Js.readonly_prop

            method windowSize : float Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        [%e div ~class_name:"fp-ScrollBar" [visible_window "bar" props##.start props##.windowSize]]
      )
