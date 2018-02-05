module M = Sxfiler_message
module T = Sxfiler_types
module FFI = Sxfiler_ffi
module Thread = Lwt

(* All state of this application *)
type t = {
  current_dir: string;
  file_list: T.File_stat.t list;
  waiting: bool;
  current_cursor: T.current_cursor;
}

class type js = object
  method currentDir: Js.js_string Js.t Js.readonly_prop
  method file_list: T.File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method currentCursor : Js.number Js.t Js.readonly_prop
end

let empty = {
  current_dir = "";
  file_list = [];
  waiting = false;
  current_cursor = 0;
}

let to_js t = object%js
  val currentDir = Js.string t.current_dir
  val file_list = List.map T.File_stat.to_js t.file_list
                  |> Array.of_list
                  |> Js.array
  val waiting = Js.bool t.waiting
  val currentCursor = float_of_int t.current_cursor |>  Js.number_of_float
end

let of_js t =
  let file_list = Js.to_array t##.file_list in
  let file_list = Array.to_list file_list
                  |> List.map T.File_stat.of_js
  in
  {
    current_dir = Js.to_string t##.currentDir;
    file_list;
    waiting = Js.to_bool t##.waiting;
    current_cursor = Js.float_of_number t##.currentCursor |> int_of_float
  }
