open Sxfiler_core
module D = Sxfiler_domain
module Gen = Sxfiler_generated.Keymap
module Tr = Sxfiler_translator

type error =
  [ `Load_error      of Protocol_conv_json.Json.error
  | `Format_error    of string
  | `Translate_error of Sxfiler_translator.Keymap.error
  | `Sys_error       of string
  | `Invalid_path    of Path.error
  ]

type save_error =
  [ `Path_is_dir of Path.t
  | `Unknown
  ]

let show_error = function
  | `Load_error err      ->
      Printf.sprintf "Keymap_file.error(Load_error(%s))" & Protocol_conv_json.Json.error_to_string_hum err
  | `Format_error err    -> Printf.sprintf "Keymap_file.error(Format_error(%s))" err
  | `Translate_error err -> Printf.sprintf "Keymap_file.error(Translate_error(%s))" & Tr.Keymap.show_error err
  | `Sys_error err       -> Printf.sprintf "Keymap_file.error(Sys_error(%s))" err
  | `Invalid_path e      -> Printf.sprintf "Keymap_file.error(Invalid_path(%s))" & Path.show_error e

let load path : (D.Keymap.t, error) result =
  let path = Path.to_string path in
  let open Result.Infix in
  let* keymap =
    try Yojson.Safe.from_file path |> Result.ok with
    | Sys_error e         -> Error (`Sys_error e)
    | Yojson.Json_error e -> Error (`Format_error e)
  in
  let* keymap = Gen.Keymap.of_json keymap |> Result.map_error (fun err -> `Load_error err) in
  Tr.Keymap.to_domain keymap |> Result.map_error (fun err -> `Translate_error err)

let save keymap path =
  let path' = Path.to_string path in
  if Sys.is_directory path' then Error (`Path_is_dir path)
  else
    let keymap = Tr.Keymap.of_domain keymap |> Gen.Keymap.to_json in
    try Yojson.Safe.to_file path' keymap |> Result.ok with _ -> Error `Unknown
