(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get_sync = struct
  (* Get_sync needs json implementation depended on package, so these module types and {!Make} module
     will helps to define rpc.
  *)
  module type Json = sig
    type t
  end

  module type S = sig
    type json
    type params = unit
    type result = json

    val name: string
  end

  module Make(J:Json) : S with type json := J.t = struct
    type params = unit

    type result = J.t
    let name = "keybindings/get/sync"
  end
end
