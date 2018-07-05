module C = Sxfiler_server_core
module State = C.Root_state

module Root = C.Statable.Make(struct
    type t = State.t

    let empty () = State.empty
  end)

module Completion = C.Statable.Make(struct
    type t = Sxfiler_server_completion.Completer.t option

    let empty () = None
  end)
