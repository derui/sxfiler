module R = Jsoo_reactjs

let base_selector = "fp-FileItem_FileName"
let directory_modifier = base_selector ^ "-directory"
let symlink_modifier = base_selector ^ "-symlink"
let marked_modifier = base_selector ^ "-marked"

let get_classname props =
  Classnames.to_string [
    base_selector, true;
    directory_modifier, props##.isDirectory;
    symlink_modifier, props##.isSymbolicLink;
  ]

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method name: string Js.readonly_prop
               method isDirectory: bool Js.readonly_prop
               method isSymbolicLink: bool Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        [%e span ~class_name:(get_classname props) [props##.name[@txt]]]
      )
