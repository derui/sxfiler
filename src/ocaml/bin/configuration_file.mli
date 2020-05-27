(** This module provides functions to load and save key map from/to file. *)

open Sxfiler_core

type error =
  [ `Load_error   of Protocol_conv_json.Json.error
  | `Sys_error    of string
  | `Invalid_path of Path.error
  ]

type save_error =
  [ `Path_is_dir of Path.t
  | `Unknown
  ]

val show_error : error -> string
(** [show_error err] prints string of [err] representation *)

val load : Path.t -> (Sxfiler_domain.Configuration_store.t, error) result
(** [load path] loads the key map stored in [path] *)

val save : Sxfiler_domain.Configuration_store.t -> Path.t -> (unit, save_error) result
(** [save keymap path] save [keymap] to [path]. This function always overwrite a file [path]. *)
