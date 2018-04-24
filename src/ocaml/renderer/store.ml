(* Very simple store implementation *)
module C = Sxfiler_common

type subscriber = C.State.t -> unit
type t = {
  mutable subscribers: subscriber list;
  mutable state: C.State.t;
}

let make () = {
  subscribers = [];
  state = C.State.empty;
}

let subscribe t subscriber = t.subscribers <- subscriber :: t.subscribers

let update t v =
  t.state <- v;
  List.iter (fun f -> f t.state) t.subscribers
