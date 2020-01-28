open Abbrev

type events = Updated of D.Configuration.t [@@deriving eq, show]

module Update : sig
  type input = D.Configuration.t

  type work_flow = input -> events list Lwt.t
  (** workflow to add a key binding for action to key map *)
end

type commands = Update of Update.input

val update : Common_step_configuration.save -> Update.work_flow
(** implementation of work flow to update configuration entirely *)
