module M = Sxfiler_modules
module Reaction = Sxfiler_message_reaction.Make(struct
    let resolve () = M.fs
  end)
module State = Sxfiler_main_state.Make(Reaction)
module F = Flux_frp

include F.Flux.Make.Default_runner(State)
