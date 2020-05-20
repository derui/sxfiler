open Sxfiler_core
module D = Sxfiler_domain
module Gen = Sxfiler_generated.Configuration
module Tr = Sxfiler_translator

type error =
  [ `Load_error   of Protocol_conv_json.Json.error
  | `Sys_error    of string
  | `Invalid_path of Path.error
  ]

type save_error =
  [ `Path_is_dir of Path.t
  | `Unknown
  ]

let show_error = function
  | `Load_error err ->
      Printf.sprintf "Configuration_file.error(Load_error(%s))" & Protocol_conv_json.Json.error_to_string_hum err
  | `Sys_error err  -> Printf.sprintf "Configuration_file.error(Sys_error(%s))" err
  | `Invalid_path e -> Printf.sprintf "Configuration_file.error(Invalid_path(%s))" & Path.show_error e

let load path : (D.Configuration_store.t, error) result =
  let path = Path.to_string path in
  let open Result.Infix in
  let* configuration = try Yojson.Basic.from_file path |> Result.ok with Sys_error e -> Error (`Sys_error e) in
  D.Configuration_store.of_json configuration |> Result.ok

let save store path =
  let path' = Path.to_string path in
  if Sys.is_directory path' then Error (`Path_is_dir path)
  else
    let json = Tr.Configuration_store.of_domain store |> List.map (Gen.Configuration.to_json %> Yojson.Safe.to_basic) in
    try Yojson.Basic.to_file path' (`List json) |> Result.ok with _ -> Error `Unknown
