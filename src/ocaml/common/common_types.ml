module FT = Jsoo_node.Fs_types

type cursor_pos = int
type sort_type =
  | Sort_name
  | Sort_size
  | Sort_date

module File_stat = struct
  type id = string
  type t = {
    id: id;
    filename: string;
    directory: string;
    link_path: string option;
    stat: FT.stat_obj Js.t;
  }

  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method filename: Js.js_string Js.t Js.readonly_prop
    method directory: Js.js_string Js.t Js.readonly_prop
    method stat: FT.stat_obj Js.t Js.readonly_prop
    method link_path: Js.js_string Js.t Js.optdef Js.readonly_prop
  end

  let equal v1 v2 = v1.id = v2.id

  let make ~filename ~directory ~link_path ~stat =
    let digest = Digest.string (directory ^ filename) |> Digest.to_hex in
    {
      id = digest;
      directory;
      filename;
      link_path;
      stat
    }

  let to_js t = object%js
    val id = Js.string t.id
    val filename = Js.string t.filename
    val directory = Js.string t.directory
    val stat = t.stat
    val link_path = let link_path = Js.Optdef.option t.link_path  in
      Js.Optdef.map link_path Js.string
  end

  let of_js js = {
    id = Js.to_string js##.id;
    directory = Js.to_string js##.directory;
    filename = Js.to_string js##.filename;
    stat = js##.stat;
    link_path = Js.Optdef.map (js##.link_path) Js.to_string |> Js.Optdef.to_option;
  }
end

module Pane = struct
  type t = {
    directory: string;
    file_list: File_stat.t array;
    focused_item: File_stat.t option;
    marked_items: (File_stat.id * File_stat.t) list;
  }

  class type js = object
    method directory: Js.js_string Js.t Js.readonly_prop
    method fileList: File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
    method focusedItem: File_stat.js Js.t Js.opt Js.readonly_prop
    method markedItems: File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
  end

  let equal = (=)

  let make ?(file_list=[||]) ?focused_item ?(marked_items=[]) ~directory () =
    {
      directory;
      file_list;
      focused_item;
      marked_items;
    }

  let toggle_mark ~item pane =
    let id = item.File_stat.id  in
    if List.mem_assoc id pane.marked_items then
      {pane with marked_items = List.remove_assoc id pane.marked_items}
    else
      {pane with marked_items = (id, item) :: pane.marked_items}

  let to_js : t -> js Js.t = fun t ->
    let file_stat_to_js (_, v) = File_stat.to_js v in
    object%js
      val fileList = Array.map File_stat.to_js t.file_list |> Js.array
      val directory = Js.string t.directory
      val focusedItem = Js.Opt.map (Js.Opt.option t.focused_item) File_stat.to_js
      val markedItems = Js.array @@ Array.of_list @@ List.map file_stat_to_js t.marked_items
    end

  let of_js : js Js.t -> t = fun js ->
    let file_stat_of_js v = let v' = File_stat.of_js v in File_stat.(v'.id, v') in
    {
      directory = Js.to_string js##.directory;
      file_list = Js.to_array js##.fileList |> Array.map File_stat.of_js;
      focused_item = Js.Opt.map js##.focusedItem File_stat.of_js |> Js.Opt.to_option;
      marked_items = Js.to_array js##.markedItems |> Array.map file_stat_of_js |> Array.to_list;
    }
end

module Operation_log = struct
  type log_type = Info | Error


  module Entry = struct
    type t = {
      timestamp: int64;
      content: string;
      log_type: log_type;
    }

    class type js = object
      method timestamp: float Js.readonly_prop
      method content: Js.js_string Js.t Js.readonly_prop
      method logType: log_type Js.readonly_prop
    end

    let make ?(log_type=Info) content =
      let timestamp = Unix.time () |> Int64.bits_of_float in
      {
        timestamp;
        content;
        log_type;
      }

    let to_js : t -> js Js.t = fun t -> object%js
      val timestamp = Int64.float_of_bits t.timestamp
      val content = Js.string t.content
      val logType = t.log_type
    end

    let of_js : js Js.t -> t = fun js -> {
        timestamp = Int64.bits_of_float js##.timestamp;
        content = Js.to_string js##.content;
        log_type = js##.logType
      }

  end

  type t = {
    entries: Entry.t list;
    max_entry_count: int;
  }

  class type js = object
    method entries: Entry.js Js.t Js.js_array Js.t Js.readonly_prop
    method maxEntryCount: int Js.readonly_prop
  end

  let default_max_entry_count = 100

  let empty = {
    entries = [];
    max_entry_count = default_max_entry_count;
  }

  let add_entry ~entry t =
    let entries = List.rev t.entries in
    let entries = if List.length entries > t.max_entry_count then List.tl entries |> List.rev
      else List.rev entries in
    {t with entries = List.rev @@ (entry :: entries)}

  let add_entry_with_content ?(log_type=Info) t content =
    add_entry ~entry:(Entry.make ~log_type content) t

  let to_js : t -> js Js.t = fun t -> object%js
    val entries = Js.array @@ Array.of_list @@ List.map Entry.to_js t.entries
    val maxEntryCount = t.max_entry_count
  end

  let of_js : js Js.t -> t = fun js -> {
      entries = List.map Entry.of_js @@ Array.to_list @@ Js.to_array js##.entries;
      max_entry_count = js##.maxEntryCount;
    }

end

type task_tag = [
  | `Copy
  | `Delete
  | `Move
  | `Rename
  | `Mkdir
]

module Task_request = struct
  type t =
    | Copy
    | Delete
    | Move
    | Rename of Js.js_string Js.t
    | Mkdir of Js.js_string Js.t

  class type js = object
    method _type: t Js.readonly_prop
  end

  let to_js t = object%js
    val _type = t
  end

  let of_js js = js##._type
end

module User_action = struct
  type t =
    | Confirm of Task_request.js Js.t
    | Cancel

  class type js = object
    method _type: t Js.readonly_prop
  end

  let to_js : t -> js Js.t = fun t -> object%js
    val _type = t
  end

  let of_js : js Js.t -> t = fun js -> js##._type
end

module Task_result = struct
  module Error = struct
    type recovery_strategy =
      | Rs_retry
      | Rs_ignore

    type t = {
      message: Operation_log.Entry.t;
      recovery_strategy: recovery_strategy;
    }

    class type js = object
      method message: Operation_log.Entry.js Js.t Js.readonly_prop
      method recoveryStrategy: recovery_strategy Js.readonly_prop
    end

    let to_js : t -> js Js.t = fun t -> object%js
      val message = Operation_log.Entry.to_js t.message
      val recoveryStrategy = t.recovery_strategy
    end

    let of_js : js Js.t -> t = fun js -> {
        message = Operation_log.Entry.of_js js##.message;
        recovery_strategy = js##.recoveryStrategy;
      }
  end

  type payload =
    | Payload_copy
    | Payload_delete
    | Payload_move
    | Payload_rename
    | Payload_mkdir

  type t = (payload, Error.t) result

  class type js = object
    method payload: payload Js.optdef Js.readonly_prop
    method error: Error.js Js.t Js.optdef Js.readonly_prop
  end

  let to_js : t -> js Js.t = fun t -> object%js
    val payload = match t with
      | Ok payload -> Js.Optdef.return payload
      | _ -> Js.Optdef.empty
    val error = match t with
      | Error e -> Js.Optdef.return @@ Error.to_js e
      | _ -> Js.Optdef.empty
  end

  let of_js: js Js.t -> t = fun js ->
    match Js.Optdef.to_option js##.payload with
    | Some v -> Ok v
    | None -> begin match Js.Optdef.to_option js##.error with
        | Some v -> Error (Error.of_js v)
        | None -> failwith "Unknown Task_result"
      end

  (** Make error with error message *)
  let of_error ?(recovery_strategy=Error.Rs_ignore) message = Error {
      Error.message = Operation_log.Entry.make ~log_type:Operation_log.Error message;
      recovery_strategy;
    }
end

module Pane_location = struct
  type t = [`Left | `Right] [@@deriving variants]
  type js = Js.js_string

  let to_js = function
    | `Left as v -> Js.string @@ Variants.to_name v
    | `Right as v -> Js.string @@ Variants.to_name v

  let of_js js =
    match Js.to_string js with
    | v when v = Variants.to_name `Left -> `Left
    | v when v = Variants.to_name `Right -> `Right
    | _ -> failwith "Unknown type"
end

type dialog_type =
  | Dialog_confirmation of task_tag
  | Dialog_name_input of task_tag
  | Dialog_jump
  | Dialog_history
[@@deriving variants]
