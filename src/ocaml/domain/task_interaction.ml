type id = Uuidm.t [@@deriving show, eq]

type t =
  { id : id
  ; task_id : Task.id
  ; accept_interactions : Task.Interaction_typ.t list }
[@@deriving show, eq]

(** A factory for [t] *)
module Factory = struct
  module type S = sig
    val create : task_id:Task.id -> accept_interactions:Task.Interaction_typ.t list -> t
  end

  module Make (Gen : Id_generator_intf.Gen_random with type id = id) : S = struct
    let create ~task_id ~accept_interactions =
      let id = Gen.generate () in
      {id; task_id; accept_interactions}
  end
end

(** Notifier provides an ability to send notification what the task need interaction *)
module Notifier = struct
  module type S = sig
    val notify : accept:Task.Interaction_typ.t list -> Task.id -> unit Lwt.t
  end
end
