module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = T.Configuration.t

  let make () = T.Configuration.default
  let reduce t _ = t
  let equal = (=)
end

module Store = C.Store.Make(State)
