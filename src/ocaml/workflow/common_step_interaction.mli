open Abbrev

type demand_decision = D.Interaction.command -> D.Interaction.event Lwt.t
(** signature to demand decision by user from work flow. *)
