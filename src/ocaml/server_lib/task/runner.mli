include module type of struct
  include Runner_intf
end

val make : unit -> (module Instance)
(** [make ()] get the new instance of runner. *)
