open Sxfiler_core

module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method filerState: S.File_list.State.t Js.readonly_prop
               method focused: bool Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let state = props##.filerState in
        let left = S.File_list.State.left state
        and right = S.File_list.State.right state in

        let to_component filer =
          let filer = filer.S.File_list.Filer.filer
          and index = filer.S.File_list.Filer.selected_item_index in
          [%c P_file_list.t ~key:("file-list_" ^ filer.T.Filer.id)
              ~props:(object%js
                val filer = filer
                val selectedItemIndex = index
                val focused = props##.focused
              end)
          ]
        in
        let components = List.filter Option.is_some [left;right]
                         |> List.map Option.get_exn
                         |> List.map to_component in
        R.fragment ~key:"file-lists" components
      )
