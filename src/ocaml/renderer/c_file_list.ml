module VL = Sxfiler_virtualized_list
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

let item_height = 18
let key_of_filelist = "currentNode"

module Component = R.Component.Make_stateful (struct
    class type t = object
      method items: T.File_stat.t array Js.readonly_prop
      method markedItems: T.File_stat.t array Js.readonly_prop
      method focusedItem: T.File_id.t option Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)(struct
    class type t = object
      method virtualizedList: T.File_stat.t VL.t Js.readonly_prop
    end
  end)

let is_item_marked items item = Array.exists T.File_stat.(equal item) items

let component =
  let item_to_pos items = function
    | None -> 0
    | Some item ->
      let items' = Array.map (fun v -> v.T.File_stat.id) items in
      Util.find_item_index ~v:item items'
  in
  let spec = R.component_spec
      ~constructor:(fun this props ->
          this##.state := object%js
            val virtualizedList = VL.make ~item_height ()
          end;
          this##.nodes := Jstable.create ())
      ~component_did_mount:(fun this ->
          let vl = this##.state##.virtualizedList in
          let open Minimal_monadic_caml.Option.Infix in
          ignore (
            R.Ref_table.find ~key:key_of_filelist this##.nodes >|=
            fun e -> let pos = item_to_pos this##.props##.items this##.props##.focusedItem in
            this##setState (object%js
              val virtualizedList = VL.update_list_height e##.clientHeight vl
                                    |> VL.update_all_items this##.props##.items
                                    |> VL.recalculate_visible_window pos
            end))
        )
      ~component_will_receive_props:(fun this props ->
          let open Minimal_monadic_caml.Option.Infix in
          let items = props##.items in
          let pos = item_to_pos items props##.focusedItem in
          this##setState (object%js
            val virtualizedList = VL.update_all_items items this##.state##.virtualizedList
                                  |> VL.recalculate_visible_window pos
          end)
        )
  in
  let render this =
    let props = this##.props in
    let vl = this##.state##.virtualizedList in
    let items = VL.get_items_in_window vl in
    let start_position = item_to_pos items props##.focusedItem in
    let children = Array.mapi (fun index item ->
        let module F = C.Types.File_stat in
        R.create_element ~key:(C.Types.File_id.to_string item.F.id) ~props:(object%js
          val item = item
          val selected = start_position = index
          val focused = props##.focused
          val marked = is_item_marked props##.markedItems item
        end) C_file_item.component
      ) items
    in
    let _ref e = R.Ref_table.add ~key:key_of_filelist ~value:e this##.nodes in
    let props = R.(element_spec ~class_name:"fp-FileList" ()) in
    R.Dom.of_tag `ul ~_ref ~props ~children
  in
  Component.make @@ spec render
