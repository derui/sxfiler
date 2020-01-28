(** This module provides functions to get some configurations from file. *)

open Sxfiler_core

(* Get config from file, but get default when some error happened *)
let get_config f config () = if Sys.file_exists config then f config else Error `Not_exists

let load_configuration option () =
  let filename = Filename.concat option.App_option.configuration option.App_option.config_file in
  let filename = Path.of_string filename |> Result.map_error (fun e -> `Invalid_path e) in
  let open Result.Infix in
  filename >>= Configuration_file.load
