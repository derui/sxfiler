include Store_intf

(** [Make(T)] gets the new module for module with state defined from {!Type}. *)
module Make(T:State_intf.S) : S with type state = T.t and type message = T.message =
struct
  type state = T.t
  type message = T.message
  type event = [`Change | `Dispatch]

  type subscriber = T.t -> unit

  type t = {
    subscribers: (event * subscriber) list;
    mutable state: T.t;
  }

  let make () = {
    subscribers = [];
    state = T.empty ();
  }

  let subscribe t ~f ~event = {t with subscribers = (event, f) :: t.subscribers}

  let get {state;_} = state

  let dispatch t message =
    let prev = t.state in
    t.state <- T.reduce t.state message;
    if prev <> t.state then
      List.filter (function
          | (`Dispatch, _) -> false
          | (`Change, _) -> true
        ) t.subscribers
      |>
      List.iter (fun (_, f) -> f t.state)
    else ();
    t

end
