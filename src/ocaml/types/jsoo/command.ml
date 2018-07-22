open Sxfiler_types.Command

(** Class is type of command and parameter set. *)
module Class = struct
  include Class

  type js = Js.number

  let to_js t = Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js js = match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown type"
    | Some v -> v

end

(* (\** {!Param_type} is type of parameter to be able to give value from other. *\)
 * module Param_type = struct
 *   type t =
 *     (\* list of nodes  *\)
 *     | List_nodes
 *     (\* only one node *\)
 *     | Single_node
 *   [@@deriving enum,show]
 * end
 *
 * (\** {!Param_def} provides simple definition of parameter such as name and type of it. *\)
 * module Param_def = struct
 *   type t = {
 *     name: string;
 *     typ: Param_type.t;
 *   }
 * end *)
