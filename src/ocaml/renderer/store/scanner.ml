open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_renderer_core

module File_list = struct
  type t = {
    scanner: D.Scanner.t;
    selected_item_index: int;
  }

  let make scanner =
    {
      scanner;
      selected_item_index = 0;
    }

  let move_index t ~direction =
    let max_index = List.length t.scanner.D.Scanner.nodes in
    match direction with
    | `Next -> {t with selected_item_index = min (pred max_index) (succ t.selected_item_index)}
    | `Prev -> {t with selected_item_index = max 0 (pred t.selected_item_index)}

end

module State = struct
  type message = C.Message.t
  type t = {
    file_lists: File_list.t Jstable.t;
    order: string * string;
    current_scanner: string;
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
    current_scanner = fst order;
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
    | C.Message.Update_scanner scanner ->
      Jstable.add t.file_lists Js.(string scanner.D.Scanner.name) @@ File_list.make scanner;
      t
    | C.Message.Move_cursor_to_next ->
      update_list t ~name:t.current_scanner ~f:(fun list -> File_list.(move_index list ~direction:`Next))
    | C.Message.Move_cursor_to_prev ->
      update_list t ~name:t.current_scanner ~f:(fun list -> File_list.(move_index list ~direction:`Prev))
    | _ -> t

  let equal _ _ = false
end

module Store = C.Store.Make(State)
