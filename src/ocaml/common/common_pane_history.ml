(** This module provides a type for histories and functions to manage them. *)

module History = struct
  type t = {
    directory: string;
    cursor_pos: Common_types.cursor_pos;
    timestamp: int64;
  }

  class type js = object
    method directory: Js.js_string Js.t Js.readonly_prop
    method cursorPos: Js.number Js.t Js.readonly_prop
    method timestamp: float Js.readonly_prop
  end

  let empty = {
    directory = "";
    cursor_pos = 0;
    timestamp = Int64.zero;
  }

  let make ~directory ~cursor_pos =
    let timestamp = Unix.time () |> Int64.of_float in
    {directory; cursor_pos; timestamp}

  let to_js t = object%js
    val directory = Js.string t.directory
    val cursorPos = Js.number_of_float @@ float_of_int t.cursor_pos
    val timestamp = Int64.to_float t.timestamp
  end

  let of_js js = {
    directory = Js.to_string js##.directory;
    cursor_pos = Js.float_of_number js##.cursorPos |> int_of_float;
    timestamp = Int64.of_float js##.timestamp;
  }
end

type t = {
  history_map: History.t Jstable.t;
  max_storeable_count: int;
}

class type js = object
  method historyMap: History.js Js.t Jstable.t Js.readonly_prop
  method maxStoreableCount: Js.number Js.t Js.readonly_prop
end

let make () = {
  history_map = Jstable.create ();
  max_storeable_count = 100;
}

let table_values table =
  let keys = Jstable.keys table in
  List.map (fun k -> (k, Jstable.find table k)) keys
  |> List.map (fun (k, v) -> (k, Js.Optdef.to_option v))
  (* All keys are contained table, so no problem get forcely with them *)
  |> List.map (fun (k, v) -> (k, Common_util.Option.get_exn v))

let clone_table table =
  let new_table = Jstable.create () in
  table_values table |> List.iter (fun (k, v) -> Jstable.add new_table k v);
  new_table

(** Remove oldest history in table. *)
let remove_oldest_history table =
  let module H = History in
  let values = table_values table in
  let sorted = List.sort (fun (_, a1) (_, a2) -> Int64.compare a1.H.timestamp a2.H.timestamp) values in
  let remove_target = match List.rev sorted with
    | [] -> None
    | hd :: _ -> Some (snd hd)
  in
  match remove_target with
  | Some v -> Jstable.remove table (Js.string v.directory)
  | None -> ()

(** Add history to history_map. *)
let add_history ~history t =
  let module H = History in
  let keys = Jstable.keys t.history_map in
  let key = Js.string history.H.directory in
  let new_table = clone_table t.history_map in

  begin match Jstable.find new_table key |> Js.Optdef.to_option with
  | Some _ -> Jstable.add new_table key history
  | None -> begin
      if List.length keys + 1 > t.max_storeable_count then
        remove_oldest_history new_table
      else ();
      Jstable.add new_table key history
    end
  end;
  {t with history_map = new_table}

let to_js t =
  let js_map = Jstable.create () in
  let keys = Jstable.keys t.history_map in
  List.map (fun k -> (k, Jstable.find t.history_map k |> Js.Optdef.to_option)) keys
  |> List.map (fun (k, v) -> (k, Common_util.Option.get_exn v))
  |> List.iter (fun (k, v) -> Jstable.add js_map k (History.to_js v));

  object%js
    val historyMap = js_map
    val maxStoreableCount = Js.number_of_float @@ float_of_int t.max_storeable_count
  end

let of_js js =
  let history_map = Jstable.create () in
  let keys = Jstable.keys js##.historyMap in
  List.map (fun k -> (k, Jstable.find js##.historyMap k |> Js.Optdef.to_option)) keys
  |> List.map (fun (k, v) -> (k, Common_util.Option.get_exn v))
  |> List.iter (fun (k, v) -> Jstable.add history_map k (History.of_js v));

  {
    history_map;
    max_storeable_count = Js.float_of_number js##.maxStoreableCount |> int_of_float;
  }

(** Restore some pane informations from history  *)
let restore_pane_info ~pane t =
  let module P = Common_types.Pane in
  let key = Js.string pane.P.directory in
  match Jstable.find t.history_map key |> Js.Optdef.to_option with
  | Some h -> P.make ~file_list:pane.P.file_list ~cursor_pos:h.History.cursor_pos
                ~directory:pane.P.directory ()
  | None -> P.make ~file_list:pane.P.file_list ~directory:pane.P.directory ()
