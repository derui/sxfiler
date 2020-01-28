module D = Sxfiler_domain

type demand_decision = D.Interaction.command -> D.Interaction.event Lwt.t
