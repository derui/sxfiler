type id = Uuidm.t

type t =
  { id : id
  ; task_id : Task.id
  ; filter_interaction : Task.interaction -> bool }

(** A factory for [t] *)
module Factory = struct
  module type S = sig
    val create : task_id:Task.id -> filter_interaction:(Task.interaction -> bool) -> t
  end

  module Make (Gen : Id_generator_intf.Gen_random with type id = id) : S = struct
    let create ~task_id ~filter_interaction =
      let id = Gen.generate () in
      {id; task_id; filter_interaction}
  end
end

(** Notifier provides an ability to send notification what the task need interaction *)
module Notifier = struct
  module type S = sig
    val notify : filter:(Task.interaction -> bool) -> Task.id -> unit Lwt.t
  end
end
