module VL = Sxfiler_virtualized_list
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

let item_height = 18
let key_of_filelist = "currentNode"

module Component = R.Component.Make_stateful (struct
    class type t = object
      method items: T.File_stat.t array Js.readonly_prop
      method selectedItem: T.File_stat.t option Js.readonly_prop
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
          let open Minimal_monadic_caml.Option.Infix in
          let ret = R.Ref_table.find ~key:key_of_filelist this##.nodes
            >|= fun e ->
            let item = this##.props##.selectedItem in
            let pos = match item with
              | None -> 0
              | Some item -> Util.find_item_index ~equal:T.File_stat.equal ~v:item this##.props##.items in
            this##setState (object%js
              val virtualizedList = VL.update_list_height e##.clientHeight vl
                                    |> VL.update_all_items this##.props##.items
                                    |> VL.recalculate_visible_window pos
            end)
          in ignore ret)
      ~component_will_receive_props:(fun this props ->
          let items = props##.items in
          let open Minimal_monadic_caml.Option.Infix in
          let pos = match this##.props##.selectedItem with
            | None -> 0
            | Some item -> Util.find_item_index ~equal:T.File_stat.equal ~v:item items
          in
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
    let start_position = match props##.selectedItem with
      | None -> 0
      | Some v -> Util.find_item_index ~equal:T.File_stat.equal ~v items
    in
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
