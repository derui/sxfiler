module FT = Jsoo_node.Fs_types

type current_cursor = int
type dialog_type =
  | Dialog_confirmation

module File_stat = struct
  type t = {
    id: string;
    filename: string;
    link_path: string option;
    stat: FT.stat_obj Js.t;
  }

  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method filename: Js.js_string Js.t Js.readonly_prop
    method stat: FT.stat_obj Js.t Js.readonly_prop
    method link_path: Js.js_string Js.t Js.optdef Js.readonly_prop
  end

  let equal v1 v2 = v1.id = v2.id

  let make ~filename ~link_path ~stat =
    let digest = Digest.string filename |> Digest.to_hex in
    {
      id = digest;
      filename;
      link_path;
      stat
    }

  let to_js t = object%js
    val id = Js.string t.id
    val filename = Js.string t.filename
    val stat = t.stat
    val link_path = let link_path = Js.Optdef.option t.link_path  in
      Js.Optdef.map link_path Js.string
  end

  let of_js js = {
    id = Js.to_string js##.id;
    filename = Js.to_string js##.filename;
    stat = js##.stat;
    link_path = Js.Optdef.map (js##.link_path) Js.to_string |> Js.Optdef.to_option;
  }
end

module Pane = struct
  type t = {
    directory: string;
    file_list: File_stat.t list;
    cursor_pos: current_cursor;
  }

  class type js = object
    method directory: Js.js_string Js.t Js.readonly_prop
    method fileList: File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
    method cursorPos : Js.number Js.t Js.readonly_prop
  end

  let equal = (=)

  let make ?(file_list=[]) ?(cursor_pos=0) ~directory () =
    {
      directory;
      file_list;
      cursor_pos;
    }

  let to_js : t -> js Js.t = fun t -> object%js
    val fileList = List.map File_stat.to_js t.file_list |> Array.of_list |> Js.array
    val directory = Js.string t.directory
    val cursorPos = Js.number_of_float @@ float_of_int t.cursor_pos
  end

  let of_js : js Js.t -> t = fun js -> {
      directory = Js.to_string js##.directory;
      file_list = Js.to_array js##.fileList |> Array.map File_stat.of_js |> Array.to_list;
      cursor_pos = int_of_float @@ Js.float_of_number js##.cursorPos;
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
    {t with entries}

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

module Operation_result = struct
  type recovery_strategy =
    | Rs_retry
    | Rs_ignore

  type error = {
      message: Operation_log.Entry.t;
      recovery_strategy: recovery_strategy;
    }
  type 'a t = ('a, error) result

  (** Make error with error message *)
  let of_error ?(recovery_strategy=Rs_ignore) message = Error {
      message = Operation_log.Entry.make ~log_type:Operation_log.Error message;
      recovery_strategy;
    }
end
