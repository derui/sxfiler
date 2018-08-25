module C = Sxfiler_renderer_core

(** Mocking interface for dispatcher *)
module type Dispatch = sig
  val dispatch : C.Message.t -> unit
end

(** dummy dispatcher instance with mocking interface *)
module Dummy_dispatcher(D:Dispatch) : C.Dispatcher.Instance = struct
  module Dispatcher : C.Dispatcher.S with type config = unit = struct
    type t = unit

    type config = unit
    let create () = ()
    let dispatch _ m = D.dispatch m
  end

  let this : Dispatcher.t = Dispatcher.create ()
end
