(* Very simple store implementation *)
module T = Sxfiler_types

type subscriber = State.t -> unit
type t = {
  mutable subscribers: subscriber list;
  mutable state: State.t;
}

let make () = {
  subscribers = [];
  state = State.empty ();
}

let subscribe t subscriber = t.subscribers <- subscriber :: t.subscribers

let get t = t.state

let update t v =
  t.state <- v;
  List.iter (fun f -> f t.state) t.subscribers
