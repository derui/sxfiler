module FT = Jsoo_node.Fs_types

type cursor_pos = int
type sort_type =
  | Sort_name
  | Sort_size
  | Sort_date

type file_id = string

module File_stat = struct
  type t = {
    id: file_id;
    filename: string;
    directory: string;
    link_path: string option;
    mode: int32;
    uid: int;
    gid: int;
    atime: int64;
    ctime: int64;
    mtime: int64;
    size: int64;
    is_directory: bool;
    is_file: bool;
    is_symlink: bool;
  }

  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method filename: Js.js_string Js.t Js.readonly_prop
    method directory: Js.js_string Js.t Js.readonly_prop
    method linkPath: Js.js_string Js.t Js.opt Js.readonly_prop
    method mode: Js.number Js.t Js.readonly_prop
    method uid: int Js.readonly_prop
    method gid: int Js.readonly_prop
    method atime: int64 Js.readonly_prop
    method ctime: int64 Js.readonly_prop
    method mtime: int64 Js.readonly_prop
    method size: int64 Js.readonly_prop
    method isDirectory: bool Js.t Js.readonly_prop
    method isFile: bool Js.t Js.readonly_prop
    method isSymlink: bool Js.t Js.readonly_prop
  end

  let equal v1 v2 = v1.id = v2.id

  let make ~id ~filename ~directory ~link_path ~mode
      ~uid
      ~gid
      ~atime
      ~ctime
      ~mtime
      ~size
      ~is_directory
      ~is_file
      ~is_symlink
    =
    {
      id;
      directory;
      filename;
      link_path;
      mode;
      uid;
      gid;
      atime;
      ctime;
      mtime;
      size;
      is_directory;
      is_file;
      is_symlink;
    }

  let of_js : js Js.t -> t = fun js -> {
    id = Js.to_string js##.id;
    filename = Js.to_string js##.filename;
    directory = Js.to_string js##.directory;
    link_path = Js.Opt.map (js##.linkPath) Js.to_string |> Js.Opt.to_option;
    mode = Js.float_of_number (js##.mode) |> Int32.of_float;
    uid = js##.uid;
    gid = js##.gid;
    atime = js##.atime;
    ctime = js##.ctime;
    mtime = js##.mtime;
    size = js##.size;
    is_directory = Js.to_bool js##.isDirectory;
    is_file = Js.to_bool js##.isFile;
    is_symlink = Js.to_bool js##.isSymlink;
  }
end

module Pane = struct
  type index = int
  type t = {
    directory: string;
    file_list: File_stat.t array;
    focused_item: file_id option;
    marked_items: file_id list;
  }

  class type js = object
    method directory: Js.js_string Js.t Js.readonly_prop
    method fileList: File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
    method focusedItem: Js.js_string Js.t Js.opt Js.readonly_prop
    method markedItems: Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  end

  let equal = (=)

  let make ?(file_list=[||]) ?focused_item ?(marked_items=[]) ~directory () =
    {
      directory;
      file_list;
      focused_item;
      marked_items;
    }

  let of_js : js Js.t -> t = fun js ->
    {
      directory = Js.to_string js##.directory;
      file_list = Js.to_array js##.fileList |> Array.map File_stat.of_js;
      focused_item = Js.Opt.map js##.focusedItem Js.to_string |> Js.Opt.to_option;
      marked_items = Js.to_array js##.markedItems |> Array.map Js.to_string |> Array.to_list;
    }

  let is_focused ~id pane = match pane.focused_item with
    | None -> false
    | Some id' -> id = id'

  let find_item ~id pane =
    List.find_opt (fun s -> id = s.File_stat.id) @@ Array.to_list pane.file_list

  let index_item ~id pane =
    let rec find ary v ind =
      if Array.length ary <= ind then 0
      else if v = ary.(ind).File_stat.id then ind
      else find ary v (succ ind)
    in
    find pane.file_list id 0

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
  | `Change_permission
]

module Task_request = struct
  type t =
    | Copy
    | Delete
    | Move
    | Rename of Js.js_string Js.t
    | Mkdir of Js.js_string Js.t
    | Change_permission of int

  class type js = object
    method _type: t Js.readonly_prop
  end

  let to_js t = object%js
    val _type = t
  end

  let of_js js = js##._type
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
    | Payload_change_permission

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
  type t = [`Left | `Right]
  type js = Js.js_string

  let of_js js =
    match Js.to_string js with
    | v when v = "left" -> `Left
    | v when v = "right" -> `Right
    | _ -> failwith "Unknown type"
end

module Dialog_type = struct
  type t =
    | Confirmation of {
        title: string;
        on_complete: unit -> Common_message.t;
        content: string;
      }
    | Name_input of {
        title: string;
        on_execute: Js.js_string Js.t -> Common_message.t;
      }
    | Jump
    | History
    | Change_permission
end
