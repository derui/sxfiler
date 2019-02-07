type id = string [@@deriving show]

module Operation = struct
  type t =
    | Move of {source : Filer.t; dest : Filer.t}
    | Copy of {source : Filer.t; dest : Filer.t}
    | Delete of {target : Filer.t}
    | Change_mode of {target : Filer.t}
  [@@deriving show]
end

(** [node_plan] is difference between current nodes and simulated it. *)
type node_plan =
  { operation : Operation.t
  ; node : Node.t }
[@@deriving show]

(** [t] is result of plan. *)
type t =
  { id : id
  ; operation : Operation.t
  ; nodes : Node.t list }
[@@deriving show]

(** [make ~id ~operation ~nodes] makes new plan [t] instance from. *)
let make ~id ~operation ~nodes = {id; operation; nodes}

(** Factory interface for [type: t] *)
module Factory = struct
  module type Id_generator = sig
    val generate : unit -> id
  end

  module type S = sig
    val create : operation:Operation.t -> nodes:Node.t list -> t
  end

  module Make (G : Id_generator) : S = struct
    let create ~operation ~nodes =
      let id = G.generate () in
      make ~operation ~id ~nodes
  end
end
