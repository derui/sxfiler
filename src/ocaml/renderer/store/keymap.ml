module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = string T.Key_map.t

  let make () = T.Key_map.make ()
  let reduce t = function
    | C.Message.Update_keymap keymap -> keymap
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
