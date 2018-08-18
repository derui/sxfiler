module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = unit

  let make () = ()

  let reduce t = function
    | C.Message.Switch_mode mode -> begin match mode with
        | C.Types.Mode.File_tree -> t
      end
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
