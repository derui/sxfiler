(** this module defines functions for procedures for keymap. *)

module G = Sxfiler_server_gateway
module P = Procedure

(* defines procedure to get current key bindings *)
module Get_spec (G : G.Keymap.Get.S) : P.Spec = struct
  module Gateway = G

  let method_ = "keymap/get"
  let param_requirement = `Not_required ()
end

(* defines procedure to get current key bindings *)
module Reload_spec (G : G.Keymap.Reload.S) : P.Spec = struct
  module Gateway = G

  let method_ = "keymap/reload"
  let param_requirement = `Not_required ()
end
