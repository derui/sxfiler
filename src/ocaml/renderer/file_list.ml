module VL = Sxfiler_virtualized_list
module C = Sxfiler_common
module T = C.Types
module R = Reactjscaml

let item_height = 18
let key_of_filelist = "currentNode"

module Component = R.Component.Make_stateful (struct
    class type t = object
      method items: T.File_stat.t list Js.readonly_prop
      method cursor_pos: T.current_cursor Js.readonly_prop
    end
  end)(struct
    class type t = object
      method virtualized_list: T.File_stat.t VL.t Js.readonly_prop
    end
  end)

let component = Component.make R.Core.Component_spec.({
    empty with
    initialize = Some (fun this props ->
        this##.state := object%js
          val virtualized_list = VL.make ~item_height ()
        end;
        this##.nodes := Jstable.create ();
      );
    component_did_mount = Some (fun this ->
        let vl = this##.state##.virtualized_list in
        match R.Ref_table.find ~key:key_of_filelist this##.nodes with
        | None -> ()
        | Some e -> begin
            this##setState (object%js
              val virtualized_list = VL.update_list_height e##.clientHeight vl
                                     |> VL.update_all_items (Array.of_list this##.props##.items)
                                     |> VL.recalculate_visible_window this##.props##.cursor_pos
            end)
          end;
      );
    component_will_receive_props = Some (fun this props ->
        let items = Array.of_list props##.items in
        this##setState (object%js
          val virtualized_list = VL.update_all_items items this##.state##.virtualized_list
                                 |> VL.recalculate_visible_window props##.cursor_pos
        end);
        true
      );
    render = (fun this ->
        let props = this##.props in
        let vl = this##.state##.virtualized_list in
        let items = VL.get_items_in_window vl
        and start_position = props##.cursor_pos - VL.start_position_of_window vl in
        let children = Array.mapi (fun index item ->
            let module F = C.Types.File_stat in
            R.element ~key:item.F.id ~props:(object%js
              val item = item
              val selected = start_position = index
            end) File_item.component
          ) items
        in
        R.Dom.of_tag `ul ~_ref:(fun e -> R.Ref_table.add ~key:key_of_filelist ~value:e this##.nodes)
          ~props:R.Core.Element_spec.({
              empty with class_name = Some (Classnames.(return "fp-FileList" |> to_string))
            })
          ~children
      )
  })
