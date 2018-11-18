module D = Sxfiler_domain
module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module Tr = Sxfiler_renderer_translator

module State = struct
  type message = C.Message.t
  type t = T.Key_map.t

  let make () = T.Key_map.empty
  let reduce t = function C.Message.Update_keymap keymap -> keymap | _ -> t
  let equal = ( = )
end

module Store = C.Store.Make (State)
