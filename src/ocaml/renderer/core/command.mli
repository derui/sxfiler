(** This module defines base signatures for command and registry of it.

    All commands in this application must define with signatures in this module, and
    must fulfill requirements of signatures.
*)
include module type of (struct include Command_intf end)

(** {!Registry} provides set of functions to manage command. *)
module Registry : sig
  type t

  (** [make ()] returns new instance of Registry *)
  val make: unit -> t

  (** [register t command] add a command to registry [t]. Overwrite old command if give the command that has same name. *)
  val register: t -> (module Instance) -> t

  (** [get t ~name] returns the command having [name]. *)
  val get: t -> name:string -> (module Instance) option

end
