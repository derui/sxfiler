
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method scannerState: S.Scanner.State.t Js.readonly_prop
               method focused: bool Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let state = props##.scannerState in
        let file_lists = S.Scanner.State.file_lists state in

        let to_component file_list =
          let scanner = file_list.S.Scanner.File_list.scanner in
          [%c P_file_list.t ~key:("file-list_" ^ scanner.T.Scanner.id)
              ~props:(object%js
                val fileList = file_list
                val focused = props##.focused
              end)
          ]
        in

        R.fragment ~key:"file-lists" @@ List.map to_component file_lists
      )
