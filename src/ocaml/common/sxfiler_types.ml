module FFI = Sxfiler_ffi

type current_cursor = int

module File_stat = struct
  type t = {
    id: string;
    filename: string;
    link_path: string option;
    stat: FFI.Fs.stat_obj Js.t;
  }

  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method filename: Js.js_string Js.t Js.readonly_prop
    method stat: FFI.Fs.stat_obj Js.t Js.readonly_prop
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

module Pane_id : sig
  type t
  type js = Js.js_string

  val make: unit -> t

  val equal: t -> t -> bool
  val to_js: t -> js Js.t
  val of_js: js Js.t -> t

  val to_string: t -> string

end = struct
  type t = string
  type js = Js.js_string

  let make () = Uuidm.(v `V4 |> to_string)

  let equal = ( = )

  let to_js = Js.string
  let of_js = Js.to_string

  let to_string t = t
end

module Pane = struct
  type t = {
    id: Pane_id.t;
    directory: string;
    file_list: File_stat.t list;
    cursor_pos: current_cursor;
  }

  class type js = object
    method id: Pane_id.js Js.t Js.readonly_prop
    method directory: Js.js_string Js.t Js.readonly_prop
    method fileList: File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
    method cursorPos : Js.number Js.t Js.readonly_prop
  end

  let make ?(file_list=[]) ?(cursor_pos=0) ~id ~directory () =
    {
      id;
      directory;
      file_list;
      cursor_pos;
    }

  let to_js : t -> js Js.t = fun t -> object%js
    val id = Pane_id.to_js t.id
    val fileList = List.map File_stat.to_js t.file_list |> Array.of_list |> Js.array
    val directory = Js.string t.directory
    val cursorPos = Js.number_of_float @@ float_of_int t.cursor_pos
  end

  let of_js : js Js.t -> t = fun js -> {
      id = Pane_id.of_js js##.id;
      directory = Js.to_string js##.directory;
      file_list = Js.to_array js##.fileList |> Array.map File_stat.of_js |> Array.to_list;
      cursor_pos = int_of_float @@ Js.float_of_number js##.cursorPos;
    }
end
