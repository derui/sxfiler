
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless (struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
      method viewerState: S.Viewer_stacks.File_tree.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)

let t = Component.make (fun props ->
    let state = props##.viewerState in
    let trees = S.Viewer_stacks.File_tree.to_list state in

    let to_component tree =
      let scanner = tree.S.Viewer_stacks.File_tree.scanner in
      [%c C_file_list.t ~key:("file-list_" ^ scanner.T.Scanner.name)
          ~viewerState:tree
          ~focused:props##.focused
      ]
    in

    R.fragment ~key:"file-lists" @@ List.map to_component trees
  )
