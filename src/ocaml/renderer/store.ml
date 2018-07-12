open Store_intf

(** [Make(T)] gets the new module for module with state defined from {!Type}. *)
module Make(T:State_intf.S) : S with type state = T.t and type message = T.message =
struct
  type message = T.message
  type state = T.t
  type subscriber = state -> unit
  type t = {
    subscribers: subscriber list;
    mutable state: state;
  }

  let make () = {
    subscribers = [];
    state = T.empty ();
  }

  let subscribe t subscriber = {t with subscribers = subscriber :: t.subscribers}

  let get {state;_} = state

  let update t message =
    t.state <- T.update t.state message;
    List.iter (fun f -> f t.state) t.subscribers;
    t

end

module Viewer_stacks_store = Make(State.Viewer_stacks)
let viewer_stack : Viewer_stacks_store.t Tag.def = Tag.def ~name:"viewer_stack"

module Config_store = Make(State.Config)
let config : Config_store.t Tag.def = Tag.def ~name:"config"
