module D = Sxfiler_domain
module C = Sxfiler_server_core
module T = Sxfiler_server_task

module Root = C.Statable.Make(struct
    type t = C.Root_state.t

    let empty () = C.Root_state.empty
  end)

module Completion = C.Statable.Make(struct
    type t = (module Sxfiler_server_completion.Completer.Instance)

    (* initial value is truly dummy implementation. *)
    let empty () = (module struct
                     module Completer = struct
                       type t = string
                       let read _ ~input:_ ~collection:_ ~stringify:_ = []

                     end
                     let instance = "not initialized"
                   end : Sxfiler_server_completion.Completer.Instance)
  end)

(* Cached source to complete in next operation. *)
module Cached_source = C.Statable.Make(struct
    type t = Sxfiler_domain.Completion.collection
    let empty () = []
  end)

module Task_runner : sig
  val get: unit -> (module T.Runner.Instance)
end = struct
  let t = ref None

  let get () =
    match !t with
    | None -> let v = T.Runner.make () in
      t := Some v;
      v
    | Some t -> t
end

module Keymap = C.Statable.Make(struct
    type t = string D.Key_map.t

    let empty () = D.Key_map.empty
  end)

module Configuration = C.Statable.Make(struct
    type t = Sxfiler_domain.Configuration.t

    let empty () = Sxfiler_domain.Configuration.default
  end)
