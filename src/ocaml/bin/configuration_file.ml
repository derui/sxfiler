open Sxfiler_core
module D = Sxfiler_domain
module Gen = Sxfiler_generated.Configuration
module Tr = Sxfiler_translator

type error =
  [ `Load_error      of Protocol_conv_json.Json.error
  | `Translate_error of Sxfiler_translator.Configuration.error
  | `Sys_error       of string
  | `Invalid_path    of Path.error
  ]

type save_error =
  [ `Path_is_dir of Path.t
  | `Unknown
  ]

let show_error = function
  | `Load_error err      ->
      Printf.sprintf "Configuration_file.error(Load_error(%s))" & Protocol_conv_json.Json.error_to_string_hum err
  | `Translate_error err ->
      Printf.sprintf "Configuration_file.error(Translate_error(%s))" & Tr.Configuration.show_error err
  | `Sys_error err       -> Printf.sprintf "Configuration_file.error(Sys_error(%s))" err
  | `Invalid_path e      -> Printf.sprintf "Configuration_file.error(Invalid_path(%s))" & Path.show_error e

let load path : (D.Configuration.t, error) result =
  let path = Path.to_string path in
  let open Result.Infix in
  let* keymap = try Yojson.Safe.from_file path |> Result.ok with Sys_error e -> Error (`Sys_error e) in
  let* configuration = Gen.Configuration.of_json keymap |> Result.map_error (fun err -> `Load_error err) in
  Tr.Configuration.to_domain configuration |> Result.map_error (fun err -> `Translate_error err)

let save keymap path =
  let path' = Path.to_string path in
  if Sys.is_directory path' then Error (`Path_is_dir path)
  else
    let keymap = Tr.Configuration.of_domain keymap |> Gen.Configuration.to_json in
    try Yojson.Safe.to_file path' keymap |> Result.ok with _ -> Error `Unknown
