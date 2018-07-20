module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    config: T.Configuration.t;
    mode: C.Types.Mode.t;
  }

  let make () = {
    config = T.Configuration.default;
    mode = C.Types.Mode.File_tree;
  }
  let reduce t _ = t
  let equal = (=)

  let mode {mode;_} = mode
end

module Store = C.Store.Make(State)
