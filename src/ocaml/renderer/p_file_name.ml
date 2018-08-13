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

let resolve_name props =
  let base_dir = props##.parentDirectory
  and path = props##.path in
  String.sub path (String.length base_dir) (String.length path - String.length base_dir)

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method parentDirectory: string Js.readonly_prop
               method path: string Js.readonly_prop
               method isDirectory: bool Js.readonly_prop
               method isSymbolicLink: bool Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let name = resolve_name props in
        [%e span ~class_name:(get_classname props) [name[@txt]]]
      )
