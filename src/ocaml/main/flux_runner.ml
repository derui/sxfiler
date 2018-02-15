module M = Modules
module Reaction = Message_reaction.Make(struct
    let resolve () = M.fs
  end)

module C = Sxfiler_common
module S = C.State
module T = C.Types

module Make(Reaction:Message_reaction.S) : Flux_frp.Flux.S.State
  with module Thread = Lwt
   and type message = C.Message.t
   and type t = S.t = struct

  module Thread = Lwt

  type t = S.t

  type message = C.Message.t
  type command = message Thread.t

  let equal = ( = )
  let update = Reaction.react
end

module State = Make(Reaction)

include Flux_frp.Flux.Make.Default_runner(State)
