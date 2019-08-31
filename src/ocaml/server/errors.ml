module G = Sxfiler_server_gateway
(** this module defines application error raises from JSON-RPC. *)

module Jye = Jsonrpc_yojson.Error
module E = Jsonrpc.Types.Error_code

let of_gateway_error = function
  | G.Gateway_error.Unknown_error v ->
    raise
      Jye.(Jsonrpc_error (make (E.make ~message:Printf.(sprintf "Unknown error: %s" v) (-1))))
  | Filer_already_exists ->
    raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Filer already exists" (-3)))
  | Filer_not_found -> raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Filer not found" (-2)))
  | Filer_not_directory ->
    raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Filer not directory" (-5)))
  | Item_not_found -> raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Item not found" (-4)))
  | Task_not_found -> raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Task not found" (-6)))
  | Filer_same_filer -> raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Same filer" (-7)))
  | Bookmark_conflict ->
    raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Same path in bookmark" (-8)))
  | Bookmark_not_found ->
    raise Jye.(Jsonrpc_error (make @@ E.make ~message:"Not found given bookmark" (-9)))
