module C = Sxfiler_server_core

module Root = C.Statable.Make(struct
    type t = C.Root_state.t

    let empty () = C.Root_state.empty
  end)

module Completion = C.Statable.Make(struct
    type t = Sxfiler_server_completion.Completer.t option

    let empty () = None
  end)
