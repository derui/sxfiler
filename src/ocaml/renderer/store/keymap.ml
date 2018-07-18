module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = C.Key_map.t

  let make () = C.Key_map.empty
  let reduce t = function
    | C.Message.Update_keymap keymap -> keymap
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
