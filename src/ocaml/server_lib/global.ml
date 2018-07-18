module C = Sxfiler_server_core
module T = Sxfiler_server_task

module Root = C.Statable.Make(struct
    type t = C.Root_state.t

    let empty () = C.Root_state.empty
  end)

module Completion = C.Statable.Make(struct
    type t = Sxfiler_server_completion.Completer.t option

    let empty () = None
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

module Keybindings = C.Statable.Make(struct
    type t = Yojson.Safe.json

    let empty () = `Null
  end)
