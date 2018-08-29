include Store_intf

(** [Make(T)] gets the new module for module with state defined from {!Type}. *)
module Make (T : State_intf.S) : S with type state = T.t and type message = T.message = struct
  type state = T.t
  type message = T.message
  type subscriber = T.t -> unit

  type t =
    { mutable subscribers : subscriber list
    ; mutable state : T.t }

  let make state = {subscribers = []; state}
  let subscribe t ~f = t.subscribers <- f :: t.subscribers
  let get {state; _} = state

  let dispatch t message =
    let prev = t.state in
    t.state <- T.reduce t.state message ;
    if not @@ T.equal prev t.state then List.iter (fun f -> f t.state) t.subscribers else ()
end

(** [Make(T)] gets the new module for module with state defined from {!Type}. *)
module Make_group (T : State_intf.S) (G : Grouping with type state = T.t) :
  S with type state = T.t and type message = T.message = struct
  type state = T.t
  type message = T.message
  type subscriber = T.t -> unit

  type t =
    { mutable subscribers : subscriber list
    ; mutable state : T.t }

  let make state =
    let t = {subscribers = []; state} in
    G.watch_state (fun state -> t.state <- state) state ;
    t

  let subscribe t ~f = t.subscribers <- f :: t.subscribers
  let get {state; _} = state

  let dispatch t message =
    let prev = t.state in
    t.state <- T.reduce t.state message ;
    if not @@ T.equal prev t.state then List.iter (fun f -> f t.state) t.subscribers else ()
end
