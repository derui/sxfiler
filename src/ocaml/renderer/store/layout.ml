module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = string list

  let make () = []
  let reduce t _ = t
  let equal = (=)
end

module Store = C.Store.Make(State)
