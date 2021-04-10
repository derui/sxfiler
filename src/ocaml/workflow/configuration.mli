open Abbrev

include module type of Configuration_intf

val update :
  Update.input ->
  (Update.output, [> `Step_configuration_instance of (module Common_step_configuration.Instance) S.Context.t ]) S.t
