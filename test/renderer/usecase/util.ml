module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

(** Mocking interface for dispatcher *)
module type Dispatch = sig
  val dispatch : C.Message.t -> unit
end

(** dummy dispatcher instance with mocking interface *)
module Dummy_dispatcher (D : Dispatch) : C.Dispatcher.Instance = struct
  module Dispatcher : C.Dispatcher.S with type config = unit = struct
    type t = unit
    type config = unit

    let create () = ()
    let dispatch _ m = D.dispatch m
  end

  let this : Dispatcher.t = Dispatcher.create ()
end

(* Service stubs. *)
module Service_stub = struct
  module Filer : S.Filer.S = struct
    let make _ = assert false
    let get _ = assert false
    let move_parent _ = assert false
    let enter_directory _ = assert false
    let plan_move_nodes _ = assert false
  end
end
