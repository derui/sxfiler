module D = Sxfiler_domain

module type State = sig
  val get : unit -> Random.State.t
end

module Make (State : State) : D.Workbench.Factory = struct
  let id_gen = Uuidm.v4_gen (State.get ())

  let make env =
    let id = id_gen () in
    D.Workbench.make ~id ~env ~corrections:[]
end
