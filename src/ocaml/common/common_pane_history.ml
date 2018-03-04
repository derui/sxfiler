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

(** Remove oldest history in table. *)
let remove_oldest_history ?(count=1) table =
  let module H = History in
  let keys = Jstable.keys table in
  let values = List.map (Jstable.find table) keys
               |> List.map (Js.Optdef.to_option)
               (* All keys are contained table, so no problem get forcely with them *)
               |> List.map (Common_util.Option.get_exn) in
  let sorted = List.sort (fun a1 a2 -> Int64.compare a1.H.timestamp a2.H.timestamp) values in
  let remove_target = match List.rev sorted with
    | [] -> None
    | hd :: _ -> Some hd
  in
  match remove_target with
  | Some v -> Jstable.remove table (Js.string v.directory)
  | None -> ()

(** Add history to history_map. *)
let add_history ~history t =
  let module H = History in
  let keys = Jstable.keys t.history_map in
  let key = Js.string history.H.directory in

  match Jstable.find t.history_map key |> Js.Optdef.to_option with
  | Some _ -> Jstable.add t.history_map key history
  | None -> begin
      if List.length keys + 1 > t.max_storeable_count then
        remove_oldest_history t.history_map
      else ();
      Jstable.add t.history_map key history
    end

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
