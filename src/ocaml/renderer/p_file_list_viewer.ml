open Sxfiler_core
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method filerState : S.File_list.State.t Js.readonly_prop

            method focused : bool Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let state = props##.filerState in
        let left = S.File_list.State.(left state, is_current state ~pos:`Left)
        and right = S.File_list.State.(right state, is_current state ~pos:`Right) in
        let to_component (filer, focused) =
          let location = filer.S.File_list.Filer.location
          and id = filer.id
          and nodes = filer.nodes
          and index = filer.selected_item_index in
          [%c
            P_file_list.t ~key:("file-list_" ^ id)
              ~props:
                (object%js
                  val location = location

                  val nodes = nodes

                  val selectedItemIndex = index

                  val focused = props##.focused && focused
                end)]
        in
        let components =
          let open Fun in
          List.filter (fst %> Option.is_some) [left; right]
          |> List.map (fun (v, focused) -> (Option.get_exn v, focused))
          |> List.map to_component
        in
        R.fragment ~key:"file-lists" components )
