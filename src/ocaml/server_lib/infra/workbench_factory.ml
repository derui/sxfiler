module D = Sxfiler_domain

module type State = sig
  val get : unit -> Random.State.t
end

module Make (State : State) : D.Workbench.Factory = struct
  let make env =
    let id = Uuidm.v4_gen (State.get ()) () in
    D.Workbench.make ~id ~env ~corrections:[]
end
