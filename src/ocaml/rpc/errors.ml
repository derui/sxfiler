(** this module defines application error raises from JSON-RPC. *)
module E = Jsonrpc_ocaml.Types.Error_code

(** Errors for filer module. *)
module Filer = struct
  let not_found = E.make ~message:"filer:Not_found" (-1)
  let already_exists = E.make ~message:"filer:Already_exists" (-2)
end
