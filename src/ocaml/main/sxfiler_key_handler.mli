
(** The type of key handler function *)
type handler = unit -> Sxfiler_common.Message.t

(** The module to map key and handler function.  *)
module Handler_map : Map.S with type key = Sxfiler_kbd.t

type handler_map = handler Handler_map.t

(** Return an empty handler mapping *)
val empty : handler_map

(** Add a new mapping with key and handler function. *)
val add_handler :
  handlers:handler_map ->
  key:Handler_map.key ->
  handler:handler -> handler_map

(** Remove a handler mapped with given key. *)
val remove_handler :
  handlers:handler_map ->
  key:Handler_map.key ->
  handler_map

(** Dispatch key to handler mappings. Return None if no any handlers are available
    given key in argument.
*)
val dispatch : handlers:handler_map -> key:Handler_map.key -> Sxfiler_common.Message.t option
