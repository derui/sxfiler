(** this module defines application error raises from JSON-RPC. *)
module E = Jsonrpc_ocaml.Types.Error_code

let unknown_error v = E.make ~message:Printf.(sprintf "Unknown error: %s" v) (-1)
let filer_not_found = E.make ~message:"Filer not found" (-2)
let filer_already_exists = E.make ~message:"Filer already exists" (-3)
let node_not_found = E.make ~message:"Node not found" (-4)
let filer_not_directory = E.make ~message:"Filer not directory" (-5)
let plan_not_found = E.make ~message:"Plan not found" (-6)
let plan_same_filer = E.make ~message:"Plan can not accept same filer for this plan" (-6)
