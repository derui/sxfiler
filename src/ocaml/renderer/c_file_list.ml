open Sxfiler_core
module VL = Sxfiler_virtualized_list
module T = Sxfiler_types
module R = Jsoo_reactjs

let item_height = 18
let key_of_filelist = "currentNode"

module Component = R.Component.Make_stateful (struct
    class type t = object
      method viewerState: Types.Viewer.File_tree.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)(struct
    class type t = object
      method virtualizedList: T.Node.t VL.t Js.readonly_prop
    end
  end)

let component =
  let module Vt = Types.Viewer.File_tree in
  let spec = R.component_spec
      ~constructor:(fun this _ ->
          this##.state := object%js
            val virtualizedList = VL.make ~item_height ()
          end;
          this##.nodes := Jstable.create ())
      ~component_did_mount:(fun this ->
          let vt = this##.props##.viewerState in
          let vl = this##.state##.virtualizedList in
          let open Option.Infix in
          ignore (
            R.Ref_table.find ~key:key_of_filelist this##.nodes >|= fun e ->
            let pos = vt.Vt.selected_item_index in
            this##setState (object%js
              val virtualizedList = VL.update_list_height e##.clientHeight vl
                                    |> VL.update_all_items @@ Array.of_list vt.Vt.snapshot.nodes
                                    |> VL.recalculate_visible_window pos
            end))
        )
      ~component_will_receive_props:(fun this _ ->
          let vt = this##.props##.viewerState in
          ignore (
            let items = Array.of_list vt.Vt.snapshot.nodes in
            let vl = VL.update_all_items items this##.state##.virtualizedList in
            let vl = VL.recalculate_visible_window vt.Vt.selected_item_index vl
            in
            this##setState (object%js
              val virtualizedList = vl
            end)
          )
        )
  in
  let render this =
    let vt = this##.props##.viewerState in
    let vl = this##.state##.virtualizedList in

    let items = VL.get_items_in_window vl in

    let children = Array.mapi (fun index item ->
        let module N = T.Node in
        R.create_element ~key:item.N.full_path ~props:(object%js
          val item = item
          val selected = index = vt.Vt.selected_item_index
          val focused = this##.props##.focused
        end) C_file_item.component
      ) items |> Array.to_list
    in
    let scroll_bar =
      let start, size = VL.percentage_by_visible this##.state##.virtualizedList in
      R.create_element ~key:"scroll-bar"
        ~props:(object%js
          val start = start
          val windowSize = size
        end) C_scroll_bar.component
    in
    let resize_sensor = R.create_element ~key:"resize-sensor"
        ~props:(object%js
          val getParentSize = (fun () ->
              let open Option.Infix in
              let size = R.Ref_table.find ~key:key_of_filelist this##.nodes >|= (fun e ->
                  let rect = e##getBoundingClientRect in
                  let height = int_of_float @@ rect##.bottom -. rect##.top
                  and width = int_of_float @@ rect##.right -. rect##.left in
                  {C_resize_sensor.height;width})
              in
              Firebug.console##log size;
              Option.get ~default:(fun () -> {C_resize_sensor.height = 0;width = 0}) size)
          val onResized = (fun size ->
              let height = size.C_resize_sensor.height in
              this##setState (object%js
                val virtualizedList = VL.update_list_height height vl
              end))
        end) C_resize_sensor.component
    in
    let children = List.concat [children;[resize_sensor;scroll_bar]] in
    let _ref e = R.Ref_table.add ~key:key_of_filelist ~value:e this##.nodes in
    let props = R.(element_spec ~class_name:"fp-FileList" ()) in
    R.Dom.of_tag `ul ~_ref ~props ~children
  in
  Component.make @@ spec render
