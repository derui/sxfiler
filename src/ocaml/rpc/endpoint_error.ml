open Sxfiler_core
module D = Sxfiler_domain
module G = Sxfiler_generated

module Validation_error = struct
  type t = {
    field : string;
    message : string;
  }

  let make ~field ~message = { field; message }

  let to_endpoint_error { field; message } = { G.Service.ErrorDetail.field; error_message = message }
end

module Filer_error = struct
  type t =
    | Not_initialized
    | Item_not_found      of D.File_item.Id.t
    | Location_not_exists of Path.t

  let not_initialized = Not_initialized

  let item_not_found id = Item_not_found id

  let location_not_exists p = Location_not_exists p

  let to_endpoint_error = function
    | Not_initialized          ->
        { G.Service.Error.status = -103; error_message = "Filer must initialize before other operation"; details = [] }
    | Item_not_found v         ->
        {
          G.Service.Error.status = -104;
          error_message = Printf.sprintf "Item not found in Filer: %s" (D.File_item.Id.show v);
          details = [];
        }
    | Location_not_exists path ->
        {
          G.Service.Error.status = -105;
          error_message = Printf.sprintf "Not exists the location: %s" (Path.to_string path);
          details = [];
        }
end

module Keymap_error = struct
  type t =
    | Empty_context
    | Invalid_key    of string
    | Invalid_keymap

  let empty_context = Empty_context

  let invalid_key s = Invalid_key s

  let invalid_keymap = Invalid_keymap

  let to_endpoint_error = function
    | Empty_context   -> { G.Service.Error.status = -101; error_message = "Empty context"; details = [] }
    | Invalid_key key ->
        { G.Service.Error.status = -102; error_message = Printf.sprintf "Invalid key sequence %s" key; details = [] }
    | Invalid_keymap  -> { G.Service.Error.status = -103; error_message = "Invalid keymap"; details = [] }
end

module Theme_error = struct
  type t =
    | Invalid_color_format
    | Duplicated           of string
    | Not_found_theme      of string

  let invalid_color_format = Invalid_color_format

  let duplicated v = Duplicated v

  let not_found_theme v = Not_found_theme v

  let to_endpoint_error = function
    | Invalid_color_format -> { G.Service.Error.status = -201; error_message = "Invalid color format"; details = [] }
    | Duplicated _         -> { G.Service.Error.status = -202; error_message = "Duplicated"; details = [] }
    | Not_found_theme _    -> { G.Service.Error.status = -203; error_message = "Not found theme"; details = [] }
end

type t =
  | Invalid_input of Validation_error.t list
  | Filer         of Filer_error.t
  | Keymap        of Keymap_error.t
  | Theme         of Theme_error.t
  | Unknown       of string

let invalid_input errors = Invalid_input errors

let filer e = Filer e

let keymap e = Keymap e

let theme e = Theme e

let unknown e = Unknown e

(** convert [t] to the error of service *)
let to_endpoint_error = function
  | Invalid_input errors ->
      {
        G.Service.Error.status = -102;
        error_message = "Invalid input";
        details = List.map Validation_error.to_endpoint_error errors;
      }
  | Filer e              -> Filer_error.to_endpoint_error e
  | Keymap e             -> Keymap_error.to_endpoint_error e
  | Theme e              -> Theme_error.to_endpoint_error e
  | Unknown e            -> {
                              G.Service.Error.status = -1;
                              error_message = Printf.sprintf "Unknown error: %s" e;
                              details = [];
                            }
