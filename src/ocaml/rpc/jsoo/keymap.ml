module Dj = Sxfiler_domain_jsoo
module Rpc = Sxfiler_rpc

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get_sync = struct
  include Rpc.Keymap.Get_sync

  let result_of_json = Dj.Key_map.of_js ~conv:(module struct
      type t = string
      let to_json t = Js.Unsafe.coerce @@ Js.string t
      let of_json js = Js.Unsafe.coerce js |> Js.to_string
    end)
  end
