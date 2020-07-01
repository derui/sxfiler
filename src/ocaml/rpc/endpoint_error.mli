open Abbrev
open Sxfiler_core

module Validation_error : sig
  type t = private {
    field : string;
    message : string;
  }

  val make : field:string -> message:string -> t

  val to_endpoint_error : t -> G.Service.ErrorDetail.t
end

module Filer_error : sig
  type t = private
    | Not_initialized
    | Item_not_found      of D.File_item.Id.t
    | Location_not_exists of Path.t

  val not_initialized : t

  val item_not_found : D.File_item.Id.t -> t

  val location_not_exists : Path.t -> t

  val to_endpoint_error : t -> G.Service.Error.t
end

module Keymap_error : sig
  type t = private
    | Empty_context
    | Invalid_key    of string
    | Invalid_keymap

  val empty_context : t

  val invalid_key : string -> t

  val invalid_keymap : t

  val to_endpoint_error : t -> G.Service.Error.t
end

module Theme_error : sig
  type t = private
    | Invalid_color_format
    | Duplicated           of string
    | Not_found_theme      of string

  val invalid_color_format : t

  val duplicated : string -> t

  val not_found_theme : string -> t

  val to_endpoint_error : t -> G.Service.Error.t
end

type t = private
  | Invalid_input of Validation_error.t list
  | Filer         of Filer_error.t
  | Keymap        of Keymap_error.t
  | Theme         of Theme_error.t
  | Unknown       of string

val invalid_input : Validation_error.t list -> t

val filer : Filer_error.t -> t

val keymap : Keymap_error.t -> t

val theme : Theme_error.t -> t

val unknown : string -> t
(** [unknown e] gets unexpected error. *)

val to_endpoint_error : t -> G.Service.Error.t
(** convert [t] to the error of service *)
