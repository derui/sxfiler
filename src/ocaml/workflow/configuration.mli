open Abbrev

type event = Updated of D.Configuration_store.t [@@deriving eq, show]

module Update : sig
  type input = {
    key : D.Configuration_store.Key.t;
    value : Yojson.Basic.t;
  }

  type work_flow = input -> event list Lwt.t
  (** workflow to add a key binding for action to key map *)
end

type commands = Update of Update.input

val update : Common_step_configuration.load -> Common_step_configuration.save -> Update.work_flow
(** implementation of work flow to update configuration entirely *)
