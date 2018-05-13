module VL = Sxfiler_virtualized_list
module C = Sxfiler_common
module T = C.Types
module P = T.Pane
module R = Jsoo_reactjs

let item_height = 18
let key_of_filelist = "currentNode"

module Component = R.Component.Make_stateful (struct
    class type t = object
      method pane: T.Pane.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)(struct
    class type t = object
      method virtualizedList: T.File_stat.t VL.t Js.readonly_prop
    end
  end)

let is_item_marked pane item =
  List.mem item.T.File_stat.id pane.P.marked_items

let component =
  let current_focused_pos pane =
    let open Minimal_monadic_caml.Option.Infix in
    pane.P.focused_item >|= fun id -> P.index_item ~id pane in
  let spec = R.component_spec
      ~constructor:(fun this props ->
          this##.state := object%js
            val virtualizedList = VL.make ~item_height ()
          end;
          this##.nodes := Jstable.create ())
      ~component_did_mount:(fun this ->
          let pane = this##.props##.pane in
          let vl = this##.state##.virtualizedList in
          let open Minimal_monadic_caml.Option.Infix in
          ignore (
            R.Ref_table.find ~key:key_of_filelist this##.nodes >|=
            fun e -> current_focused_pos pane >|=
            fun pos ->
            this##setState (object%js
              val virtualizedList = VL.update_list_height e##.clientHeight vl
                                    |> VL.update_all_items pane.P.file_list
                                    |> VL.recalculate_visible_window pos
            end))
        )
      ~component_will_receive_props:(fun this props ->
          let pane = props##.pane in
          let open Minimal_monadic_caml.Option.Infix in
          ignore (
            current_focused_pos pane >|= fun pos ->
            this##setState (object%js
              val virtualizedList = VL.update_all_items pane.P.file_list this##.state##.virtualizedList
                                    |> VL.recalculate_visible_window pos
            end)
          )
        )
  in
  let render this =
    let pane = this##.props##.pane in
    let vl = this##.state##.virtualizedList in
    let items = VL.get_items_in_window vl in
    let children = Array.mapi (fun index item ->
        let module F = C.Types.File_stat in
        R.create_element ~key:item.F.id ~props:(object%js
          val baseDirectory = Js.string pane.P.directory
          val item = item
          val selected = P.is_focused ~id:item.F.id pane
          val focused = this##.props##.focused
          val marked = is_item_marked pane item
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
              let open Minimal_monadic_caml.Option.Infix in
              let size = R.Ref_table.find ~key:key_of_filelist this##.nodes >|= (fun e ->
                  let rect = e##getBoundingClientRect in
                  let height = int_of_float @@ rect##.bottom -. rect##.top
                  and width = int_of_float @@ rect##.right -. rect##.left in
                  C_resize_sensor.({height;width}))
              in
              C.Util.Option.get ~default:C_resize_sensor.({height = 0;width = 0}) size)
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
