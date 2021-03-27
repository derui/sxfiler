open Abbrev

type event = Updated of D.Configuration_store.t [@@deriving eq, show]

module Update = struct
  type input = {
    key : D.Configuration_store.Key.t;
    value : Yojson.Basic.t;
  }

  type work_flow =
    input ->
    ( event list Lwt.t,
      [ `Step_configuration_load of Common_step_configuration.load S.Context.t
      | `Step_configuration_save of Common_step_configuration.save S.Context.t
      ] )
    S.t
  (** workflow to add a key binding for action to key map *)
end

type commands = Update of Update.input
