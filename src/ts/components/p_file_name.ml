module R = Jsoo_reactjs

let directory_modifier base = base ^ "-directory"
let symlink_modifier base = base ^ "-symlink"

let get_classname props =
  let base = Js.to_string props##.className in
  Classnames.to_string
    [ (base, true)
    ; (directory_modifier base, props##.isDirectory)
    ; (symlink_modifier base, props##.isSymbolicLink) ]

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method name : string Js.readonly_prop

            method isDirectory : bool Js.readonly_prop

            method isSymbolicLink : bool Js.readonly_prop

            method className : Js.js_string Js.t Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let name = props##.name in
        [%e span ~class_name:(get_classname props) [(name [@txt])]] )
