open Sxfiler_core
module VL = Sxfiler_virtualized_list
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let item_height = 18
let key_of_filelist = "currentNode"

(* header of file list *)
module Header = R.Component.Make_stateless (struct
    class type t = object
      method directory: string Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)

let header = Header.make (fun props ->
    let directory = props##.directory in
    let class_name =
      let open Classnames in
      to_string ["fp-FileList_Header", true;
                 "fp-FileList_Header-focused", props##.focused]
    in
    [%e header ~class_name [directory [@txt]]]
  )

(* content of file list *)
module Content = R.Component.Make_stateful (struct
    class type t = object
      method fileList: S.Scanner.File_list.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)(struct
    class type t = object
      method virtualizedList: T.Node.t VL.t Js.readonly_prop
    end
  end)

let content =
  let module Vt = S.Scanner.File_list in
  let spec = R.component_spec
      ~constructor:(fun this _ ->
          this##.state := object%js
            val virtualizedList = VL.make ~item_height ()
          end;
          this##.nodes := Jstable.create ())
      ~component_did_mount:(fun this ->
          let vt = this##.props##.fileList in
          let vl = this##.state##.virtualizedList in
          let open Option.Infix in
          ignore (
            R.Ref_table.find ~key:key_of_filelist this##.nodes >|= fun e ->
            let pos = vt.Vt.selected_item_index in
            this##setState (object%js
              val virtualizedList = VL.update_list_height e##.clientHeight vl
                                    |> VL.update_all_items @@ Array.of_list vt.Vt.scanner.nodes
                                    |> VL.recalculate_visible_window pos
            end))
        )
      ~component_will_receive_props:(fun this _ ->
          let vt = this##.props##.fileList in
          ignore (
            let items = Array.of_list vt.Vt.scanner.nodes in
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
    let vt = this##.props##.fileList in
    let vl = this##.state##.virtualizedList in

    let items = VL.get_items_in_window vl in

    let children = Array.mapi (fun index item ->
        let module N = T.Node in
        [%c P_file_item.t ~key:(Path.to_string item.N.full_path) ~item
            ~selected:(index = vt.Vt.selected_item_index)
            ~focused:this##.props##.focused ]
      ) items |> Array.to_list
    in
    let scroll_bar =
      let start, size = VL.percentage_by_visible this##.state##.virtualizedList in
      [%c C_scroll_bar.t ~key:"scroll-bar" ~start ~windowSize:size]
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
              Option.get ~default:(fun () -> {C_resize_sensor.height = 0;width = 0}) size)
          val onResized = (fun size ->
              let height = size.C_resize_sensor.height in
              this##setState (object%js
                val virtualizedList = VL.update_list_height height vl
              end))
        end) C_resize_sensor.t
    in
    let children = List.concat [children;[resize_sensor;scroll_bar]] in
    let _ref e = R.Ref_table.add ~key:key_of_filelist ~value:e this##.nodes in
    [%e ul ~_ref ~class_name:"fp-FileList_Content"  children]
  in
  Content.make @@ spec render

(* wrapping component. *)
module Component = R.Component.Make_stateless (struct
    class type t = object
      method fileList: S.Scanner.File_list.t Js.readonly_prop
      method focused: bool Js.readonly_prop
    end
  end)

let t = Component.make (fun props ->
    let state = props##.fileList in
    let scanner = state.S.Scanner.File_list.scanner in
    [%e div ~class_name:"fp-FileList"
        [[%c header ~key:"header" ~directory:(Path.to_string scanner.location) ~focused:props##.focused];
         [%c content ~key:"file-list" ~fileList:state ~focused:props##.focused];
        ]]
  )
