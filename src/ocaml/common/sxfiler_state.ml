module M = Sxfiler_message
module T = Sxfiler_types
module FFI = Sxfiler_ffi
module Thread = Lwt

(* All state of this application *)
type t = {
  current_dir: string;
  file_list: T.File_stat.t list;
  waiting: bool;
  selected_item: T.selected_item;
}

class type js = object
  method current_dir: Js.js_string Js.t Js.readonly_prop
  method file_list: T.File_stat.js Js.t Js.js_array Js.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method selected_item : Js.number Js.t Js.readonly_prop
end

let empty = {
  current_dir = "";
  file_list = [];
  waiting = false;
  selected_item = 0;
}

let to_js t = object%js
  val current_dir = Js.string t.current_dir
  val file_list = List.map T.File_stat.to_js t.file_list
                  |> Array.of_list
                  |> Js.array
  val waiting = Js.bool t.waiting
  val selected_item =float_of_int t.selected_item |>  Js.number_of_float
end

let of_js t =
  let file_list = Js.to_array t##.file_list in
  let file_list = Array.to_list file_list
                  |> List.map T.File_stat.of_js
  in
  {
    current_dir = Js.to_string t##.current_dir;
    file_list;
    waiting = Js.to_bool t##.waiting;
    selected_item = Js.float_of_number t##.selected_item |> int_of_float
  }
