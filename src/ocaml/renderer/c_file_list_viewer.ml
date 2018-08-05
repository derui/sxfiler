
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless (struct
    class type t = object
      method scannerState: S.Scanner.State.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)

let t = Component.make (fun props ->
    let state = props##.scannerState in
    let file_lists = S.Scanner.State.file_lists state in

    let to_component file_list =
      let scanner = file_list.S.Scanner.File_list.scanner in
      [%c C_file_list.t ~key:("file-list_" ^ scanner.T.Scanner.name)
          ~fileList:file_list
          ~focused:props##.focused
      ]
    in

    R.fragment ~key:"file-lists" @@ List.map to_component file_lists
  )
