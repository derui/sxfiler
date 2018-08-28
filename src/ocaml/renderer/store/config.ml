module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {config : T.Configuration.t}

  let make () = {config = T.Configuration.default}
  let reduce t = function _ -> t
  let equal = ( == )
end

module Store = C.Store.Make (State)
