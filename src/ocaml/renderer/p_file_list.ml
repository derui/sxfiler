open Sxfiler_core
module VL = Sxfiler_virtualized_list
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let item_height = 18
let key_of_filelist = "currentNode"

(* header of file list *)
let header =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method directory : string Js.readonly_prop

            method focused : bool Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let directory = props##.directory in
        let class_name =
          let open Classnames in
          to_string [("fp-FileList_Header", true); ("fp-FileList_Header-focused", props##.focused)]
        in
        [%e header ~class_name [(directory [@txt])]] )


(* content of file list *)
let content =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method nodes: S.File_list.Filer.node array Js.readonly_prop

            method selectedItemIndex : int Js.readonly_prop

            method focused : bool Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~constructor:(fun this _ -> this##.nodes := Jstable.create ())
         ~initial_custom:(fun _ _ -> object%js end)
         ~initial_state:(fun _ _ ->
             object%js
               val virtualizedList = VL.make ~item_height ()
             end )
         ~component_did_mount:(fun this ->
             let nodes = this##.props##.nodes in
             let vl = this##.state##.virtualizedList in
             let open Option.Infix in
             ignore
               ( R.Ref_table.find ~key:key_of_filelist this##.nodes
                 >|= fun e ->
                 let pos = this##.props##.selectedItemIndex in
                 this##setState
                   (object%js
                     val virtualizedList =
                       VL.update_list_height e##.clientHeight vl
                       |> VL.update_all_items nodes
                       |> VL.recalculate_visible_window pos
                   end) ) )
         ~component_will_receive_props:(fun this new_props ->
             let items = new_props##.nodes in
             ignore
               (
                let vl = VL.update_all_items items this##.state##.virtualizedList in
                let vl = VL.recalculate_visible_window new_props##.selectedItemIndex vl in
                this##setState
                  (object%js
                    val virtualizedList = vl
                  end)) )
         (fun this ->
            let vl = this##.state##.virtualizedList in
            let items = VL.get_items_in_window vl in
            let children =
              Array.to_list items
              |> List.mapi
                (fun index (item, _) ->
                   let module N = T.Node in
                   [%c
                     P_file_item.t ~key:item.N.name
                       ~props:
                         (object%js
                           val item = item

                           val selected = index = this##.props##.selectedItemIndex

                           val focused = this##.props##.focused
                         end)] )
            in
            let scroll_bar =
              let start, size = VL.percentage_by_visible this##.state##.virtualizedList in
              [%c
                C_scroll_bar.t ~key:"scroll-bar"
                  ~props:
                    (object%js
                      val start = start

                      val windowSize = size
                    end)]
            in
            let resize_sensor =
              R.create_element ~key:"resize-sensor"
                ~props:
                  (object%js
                    val getParentSize =
                      fun () ->
                        let open Option.Infix in
                        let size =
                          R.Ref_table.find ~key:key_of_filelist this##.nodes
                          >|= fun e ->
                          let rect = e##getBoundingClientRect in
                          let height = int_of_float @@ (rect##.bottom -. rect##.top)
                          and width = int_of_float @@ (rect##.right -. rect##.left) in
                          {C_resize_sensor.height; width}
                        in
                        Option.get
                          ~default:(fun () -> {C_resize_sensor.height = 0; width = 0})
                          size

                    val onResized =
                      fun size ->
                        let height = size.C_resize_sensor.height in
                        this##setState
                          (object%js
                            val virtualizedList = VL.update_list_height height vl
                          end)
                  end)
                C_resize_sensor.t
            in
            let children = List.concat [children; [resize_sensor; scroll_bar]] in
            let _ref e = R.Ref_table.add ~key:key_of_filelist ~value:e this##.nodes in
            [%e ul ~_ref ~class_name:"fp-FileList_Content" children] ))


(* wrapping component. *)
let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method location : string Js.readonly_prop
            method nodes: S.File_list.Filer.node array Js.readonly_prop

            method selectedItemIndex : int Js.readonly_prop

            method focused : bool Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        [%e
          div ~class_name:"fp-FileList"
            [ [%c
              header ~key:"header"
                ~props:
                  (object%js
                    val directory = props##.location

                    val focused = props##.focused
                  end)]
            ; [%c
              content ~key:"file-list"
                ~props:
                  (object%js
                    val nodes = props##.nodes

                    val selectedItemIndex = props##.selectedItemIndex

                    val focused = props##.focused
                  end)] ]] )
