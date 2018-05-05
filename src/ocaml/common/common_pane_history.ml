(** This module provides a type for histories and functions to manage them. *)

module T = Common_types

module Record = struct
  type t = {
    directory: string;
    focused_item: T.file_id option;
    timestamp: int64;
  }

  class type js = object
    method directory: Js.js_string Js.t Js.readonly_prop
    method focusedItem: Js.js_string Js.t Js.opt Js.readonly_prop
    method timestamp: float Js.readonly_prop
  end

  let make ~timestamp ~directory ?focused_item () =
    {directory; focused_item; timestamp}

  let of_js js =
    {
      directory = Js.to_string js##.directory;
      focused_item = Js.Opt.map js##.focusedItem Js.to_string |> Js.Opt.to_option;
      timestamp = Int64.of_float js##.timestamp;
    }
end

type t = {
  records: Record.t array;
  max_records: int;
}

class type js = object
  method records: Record.js Js.t Js.js_array Js.t Js.readonly_prop
  method maxRecords: int Js.readonly_prop
end

let empty () = {
  records = [||];
  max_records = 100;
}

let of_js js =
  {
    records = Js.array_map Record.of_js js##.records |> Js.to_array;
    max_records = js##.maxRecords;
  }
