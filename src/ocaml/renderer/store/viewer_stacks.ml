module D = Sxfiler_domain
module C = Sxfiler_renderer_core

module File_tree = struct
  type tree = {
    scanner: D.Scanner.t;
    selected_item_index: int;
  }

  type t = {
    trees: tree Jstable.t;
    tree_order: string * string;
    current_tree: string;
  }

  let make order =
    {
      trees = Jstable.create ();
      tree_order = order;
      current_tree = fst order;
    }

  let swap_order t = {t with tree_order = Sxfiler_core.Tuple.swap t.tree_order}

  let update t ~scanner =
    Jstable.add t.trees Js.(string scanner.D.Scanner.name) {scanner; selected_item_index = 0};
    t

  let get t ~name = Jstable.find t.trees Js.(string name)
                    |> Js.Optdef.to_option

  let move_index t ~name ~direction =
    let open Sxfiler_core.Option.Infix in
    let t = get t ~name >>= (fun tree ->
        let max_index = List.length tree.scanner.D.Scanner.nodes in
        match direction with
        | `Next -> Some {tree with selected_item_index = min (pred max_index) (succ tree.selected_item_index)}
        | `Prev -> Some {tree with selected_item_index = max 0 (pred tree.selected_item_index)})
      >>= fun tree -> Jstable.add t.trees Js.(string name) tree; Some t
    in
    Sxfiler_core.Option.get_exn t

  let to_list t =
    let order1, order2 = t.tree_order in
    let list = [
      Jstable.find t.trees Js.(string order1) |> Js.Optdef.to_option;
      Jstable.find t.trees Js.(string order2) |> Js.Optdef.to_option;
    ] in
    let open Sxfiler_core.Option in
    List.filter is_some list |> List.map get_exn

end


module State = struct
  type message = C.Message.t
  type t = {
    file_tree: File_tree.t;
  }

  let make order = {file_tree = File_tree.make order;}
  let reduce t = function
    | C.Message.Update_scanner scanner ->
      let file_tree = File_tree.update t.file_tree ~scanner in
      {file_tree}
    | C.Message.Move_cursor_to_next ->
      {file_tree = File_tree.(move_index t.file_tree ~name:t.file_tree.current_tree ~direction:`Next)}
    | C.Message.Move_cursor_to_prev ->
      {file_tree = File_tree.(move_index t.file_tree ~name:t.file_tree.current_tree ~direction:`Prev)}
    | _ -> t

  let equal _ _ = false
end

module Store = C.Store.Make(State)
