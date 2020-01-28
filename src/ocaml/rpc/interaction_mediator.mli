include module type of struct
  include Interaction_mediator_intf
end

val make : (module Client.Instance) -> (module Instance)
(** [make (module Gen) (module Client)] get the new instance of observer. *)
