open Sxfiler_workflow

module type State = Statable.S with type state = Sxfiler_domain.Configuration_store.t

module type Theme_option = sig
  val theme_dir : string
  (** the directory of theme *)

  val theme_config_key : Sxfiler_domain.Configuration_store.Key.t
  (** the configuration key for theme that in configuration store *)
end

module Instance (TO : Theme_option) (S : State) : Common_step.Theme.Instance
