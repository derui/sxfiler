module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = unit

  let make () = ()

  let reduce t = function
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
