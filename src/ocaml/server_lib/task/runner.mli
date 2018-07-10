
include module type of (struct include Runner_intf end)

(** [make ()] get the new instance of runner. *)
val make: unit -> (module Instance)
