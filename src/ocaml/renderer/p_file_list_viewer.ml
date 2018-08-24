
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method filerState: S.Filer.State.t Js.readonly_prop
               method focused: bool Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let state = props##.filerState in
        let file_lists = S.Filer.State.file_lists state in

        let to_component file_list =
          let filer = file_list.S.Filer.File_list.filer in
          [%c P_file_list.t ~key:("file-list_" ^ filer.T.Filer.id)
              ~props:(object%js
                val fileList = file_list
                val focused = props##.focused
              end)
          ]
        in

        R.fragment ~key:"file-lists" @@ List.map to_component file_lists
      )
