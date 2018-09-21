open Sxfiler_core
module VL = Sxfiler_virtualized_list
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let key_of_filelist = "currentNode"
let key_of_dummy_item = "dummyItem"

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

(* dummy node to measure size of item *)
let dummy_node _ref =
  let props =
    object%js
      val forwardedRef = _ref
    end
  in
  [%c P_dummy_file_item.t ~key:"dummy" ~props []]

(* content of file list *)
let content =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method nodes : S.File_list.Filer.node array Js.readonly_prop

            method selectedItemIndex : int Js.readonly_prop

            method focused : bool Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~initial_custom:(fun _ _ ->
             let refs = R.Ref_table.create () in
             R.Ref_table.define refs ~key:key_of_filelist ;
             R.Ref_table.define refs ~key:key_of_dummy_item ;
             object%js
               val refs = refs
             end )
         ~initial_state:(fun _ _ ->
             object%js
               val virtualizedList = VL.make ()
             end )
         ~component_did_mount:(fun this ->
             let nodes = this##.props##.nodes in
             let vl = this##.state##.virtualizedList in
             let open Option in
             ignore
               ( R.Ref_table.find ~key:key_of_filelist this##.custom##.refs
                 >>= fun parent ->
                 R.Ref_table.find ~key:key_of_dummy_item this##.custom##.refs
                 >|= fun item ->
                 let height_of_element e =
                   Js.Optdef.to_option e##getBoundingClientRect##.height
                   |> Option.get ~default:(fun () -> 0.0)
                 in
                 let pos = this##.props##.selectedItemIndex in
                 this##setState
                   (object%js
                     val virtualizedList =
                       VL.update_list_height (height_of_element parent) vl
                       |> VL.update_item_height (height_of_element item)
                       |> VL.update_all_items nodes |> VL.recalculate_visible_window pos
                   end) ) )
         ~component_will_receive_props:(fun this new_props ->
             let items = new_props##.nodes in
             ignore
               (let vl = VL.update_all_items items this##.state##.virtualizedList in
                let vl = VL.recalculate_visible_window new_props##.selectedItemIndex vl in
                this##setState
                  (object%js
                    val virtualizedList = vl
                  end)) )
         (fun this ->
            let vl = this##.state##.virtualizedList in
            let items = VL.get_items_in_window vl in
            let cursor = VL.adjust_cursor_position this##.props##.selectedItemIndex vl in
            let children =
              Array.to_list items
              |> List.mapi (fun index (item, marked) ->
                  let module N = T.Node in
                  [%c
                    P_file_item.t ~key:item.N.name
                      ~props:
                        (object%js
                          val item = item

                          val marked = marked

                          val selected = index = cursor && this##.props##.focused
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
                        let open Option in
                        let size =
                          R.Ref_table.find ~key:key_of_filelist this##.custom##.refs
                          >|= fun e ->
                          let rect = e##getBoundingClientRect in
                          let height = rect##.bottom -. rect##.top
                          and width = rect##.right -. rect##.left in
                          {C_resize_sensor.height; width}
                        in
                        Option.get
                          ~default:(fun () -> {C_resize_sensor.height = 0.; width = 0.})
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
            let dummy = dummy_node (R.Ref_table.use ~key:key_of_dummy_item this##.custom##.refs) in
            let children = List.concat [children; [resize_sensor; scroll_bar; dummy]] in
            let _ref = R.Ref_table.use ~key:key_of_filelist this##.custom##.refs in
            [%e ul ~_ref ~class_name:"fp-FileList_Content" children] ))

(* wrapping component. *)
let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method location : string Js.readonly_prop

            method nodes : S.File_list.Filer.node array Js.readonly_prop

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
