(** Command module defines type and signature to define commands. *)

(** Class is type of command and parameter set. *)
module Class = struct
  type t =
    (* the command to mark nodes *)
    | Mark_nodes
    (* the command to change directory of the scanner. *)
    | Jump
  [@@deriving enum,show]
end

(** {!Param_type} is type of parameter to be able to give value from other. *)
module Param_type = struct
  type t =
    (* list of nodes  *)
    | List_nodes
    (* only one node *)
    | Single_node
    (* a string *)
    | String
  [@@deriving enum,show]
end

(** {!Param_def} provides simple definition of parameter such as name and type of it. *)
module Param_def = struct
  type t = {
    name: string;
    typ: Param_type.t;
  }
end

(** Definition of command. *)
type definition = {
  command_class: Class.t;
  param_defs: Param_def.t list;
}
