open Sxfiler_workflow
(** This module provide implementation of steps of filer that are needed IO in implementation *)

module type State = Statable.S with type state = Sxfiler_domain.Filer.t option

module Instance (S : State) : Common_step.Filer.Instance
