module P = Sxfiler_server_presenter
module Rpc = Sxfiler_rpc

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get_sync = struct
  include Rpc.Keymap.Get_sync

  let result_to_yojson = P.Key_map.to_yojson ~conv:(
      module struct
        type t = string [@@deriving yojson]
      end)
end