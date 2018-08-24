open Sxfiler_core
module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types

module File_list = struct
  type t = {
    filer: T.Filer.t;
    selected_item_index: int;
  }

  let make filer =
    {
      filer;
      selected_item_index = 0;
    }

  let move_index t ~direction =
    let max_index = List.length t.filer.T.Filer.nodes in
    match direction with
    | `Next -> {t with selected_item_index = min (pred max_index) (succ t.selected_item_index)}
    | `Prev -> {t with selected_item_index = max 0 (pred t.selected_item_index)}

end

module State = struct
  type message = C.Message.t
  type t = {
    file_lists: File_list.t Jstable.t;
    order: string * string;
    current_filer: string;
  }

  let file_lists t =
    let order1, order2 = t.order in
    let list = [
      Jstable.find t.file_lists Js.(string order1) |> Js.Optdef.to_option;
      Jstable.find t.file_lists Js.(string order2) |> Js.Optdef.to_option;
    ] in
    let open Sxfiler_core.Option in
    List.filter is_some list |> List.map get_exn

  let make order = {
    file_lists = Jstable.create ();
    order;
    current_filer = fst order;
  }

  let update_list t ~name ~f =
    let open Option.Infix in
    let name = Js.string name in
    let v = (Js.Optdef.to_option @@ Jstable.find t.file_lists name) >>= fun file_list ->
      Option.some @@ f file_list
    in
    match v with
    | None -> t
    | Some v -> Jstable.add t.file_lists name v; t

  let reduce t = function
    | C.Message.Update_filer filer ->
      Jstable.add t.file_lists Js.(string filer.T.Filer.id) @@ File_list.make filer;
      t
    | C.Message.Move_cursor_to_next ->
      update_list t ~name:t.current_filer ~f:(fun list -> File_list.(move_index list ~direction:`Next))
    | C.Message.Move_cursor_to_prev ->
      update_list t ~name:t.current_filer ~f:(fun list -> File_list.(move_index list ~direction:`Prev))
    | C.Message.Swap_filer ->
      if fst t.order = t.current_filer then {t with current_filer = snd t.order}
      else {t with current_filer = fst t.order}
    | _ -> t

  let equal _ _ = false
end

module Store = C.Store.Make(State)
