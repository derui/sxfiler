open Sxfiler_workflow

module type State = Statable.S with type state = Sxfiler_domain.Keymap.t

(** The instance of keymap *)
module Instance (S : State) : Common_step.Keymap.Instance
