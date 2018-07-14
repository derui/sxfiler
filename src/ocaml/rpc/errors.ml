(** this module defines application error raises from JSON-RPC. *)
module E = Jsonrpc_ocaml.Types.Error_code

(** Errors for scanner module. *)
module Scanner = struct
  let not_found = E.make ~message:"scanner:Not_found" (-1)
  let already_exists = E.make ~message:"scanner:Already_exists" (-2)
end
