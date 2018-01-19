(* This module provides types for message that is used as action on flux architecture *)
module T = Sxfiler_types

type t =
    REQUEST_FILES_IN_DIRECTORY of string
  | FINISH_FILES_IN_DIRECTORY of (exn option * string * T.File_stat.t array)
  | REQUEST_QUIT_APPLICATION
  | SELECT_NEXT_ITEM of int
  | SELECT_PREV_ITEM of int
[@@deriving variants]

type js = [
    `REQUEST_FILES_IN_DIRECTORY of Js.js_string Js.t
  | `FINISH_FILES_IN_DIRECTORY of (exn option * Js.js_string Js.t * T.File_stat.js Js.t Js.js_array Js.t)
  | `REQUEST_QUIT_APPLICATION
  | `SELECT_NEXT_ITEM of Js.number Js.t
  | `SELECT_PREV_ITEM of Js.number Js.t
]

let of_js = function
  | `REQUEST_FILES_IN_DIRECTORY s -> request_files_in_directory @@ Js.to_string s
  | `FINISH_FILES_IN_DIRECTORY (exn, s, ary) ->
    let ary = Js.to_array ary |> Array.map T.File_stat.of_js in
    finish_files_in_directory (exn, Js.to_string s, ary)
  | `REQUEST_QUIT_APPLICATION -> request_quit_application
  | `SELECT_MEXT_ITEM v -> select_next_item v
  | `SELECT_PREV_ITEM v -> select_prev_item v

let to_js = function
  | REQUEST_FILES_IN_DIRECTORY s -> `REQUEST_FILES_IN_DIRECTORY (Js.string s)
  | FINISH_FILES_IN_DIRECTORY (e, s, ary) ->
    let ary = Js.array @@ Array.map T.File_stat.to_js ary in
    `FINISH_FILES_IN_DIRECTORY (e, Js.string s, ary)
  | REQUEST_QUIT_APPLICATION -> `REQUEST_QUIT_APPLICATION
  | SELECT_NEXT_ITEM v -> `SELECT_NEXT_ITEM (float_of_int v |> Js.number_of_float)
  | SELECT_PREV_ITEM v -> `SELECT_PREV_ITEM (float_of_int v |> Js.number_of_float)
