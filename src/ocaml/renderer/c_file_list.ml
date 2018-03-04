module VL = Sxfiler_virtualized_list
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

let item_height = 18
let key_of_filelist = "currentNode"

module Component = R.Component.Make_stateful (struct
    class type t = object
      method items: T.File_stat.t list Js.readonly_prop
      method cursorPos: T.cursor_pos Js.readonly_prop
    end
  end)(struct
    class type t = object
      method virtualizedList: T.File_stat.t VL.t Js.readonly_prop
    end
  end)

let component =
  let spec = R.component_spec
      ~constructor:(fun this props ->
          this##.state := object%js
            val virtualizedList = VL.make ~item_height ()
          end;
          this##.nodes := Jstable.create ())
      ~component_did_mount:(fun this ->
          let vl = this##.state##.virtualizedList in
          match R.Ref_table.find ~key:key_of_filelist this##.nodes with
          | None -> ()
          | Some e -> begin
              this##setState (object%js
                val virtualizedList = VL.update_list_height e##.clientHeight vl
                                       |> VL.update_all_items (Array.of_list this##.props##.items)
                                       |> VL.recalculate_visible_window this##.props##.cursorPos
              end)
            end)
      ~component_will_receive_props:(fun this props ->
          let items = Array.of_list props##.items in
          this##setState (object%js
            val virtualizedList = VL.update_all_items items this##.state##.virtualizedList
                                   |> VL.recalculate_visible_window props##.cursorPos
          end))
  in
  let render this =
    let props = this##.props in
    let vl = this##.state##.virtualizedList in
    let items = VL.get_items_in_window vl
    and start_position = props##.cursorPos - VL.start_position_of_window vl in
    let children = Array.mapi (fun index item ->
        let module F = C.Types.File_stat in
        R.create_element ~key:item.F.id ~props:(object%js
          val item = item
          val selected = start_position = index
        end) C_file_item.component
      ) items
    in
    R.Dom.of_tag `ul ~_ref:(fun e -> R.Ref_table.add ~key:key_of_filelist ~value:e this##.nodes)
      ~props:R.(element_spec ~class_name:"fp-FileList" ())
      ~children
  in
  Component.make @@ spec render
