module M = Modules
module Reaction = Message_reaction.Make(struct
    let resolve () = M.fs
  end)
module State = Main_state.Make(Reaction)
module F = Flux_frp

include F.Flux.Make.Default_runner(State)
