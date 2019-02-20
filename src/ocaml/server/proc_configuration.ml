(** this module defines functions for procedures for keybindings. *)

module Usecase = Sxfiler_usecase
module G = Sxfiler_server_gateway
module I = Sxfiler_server_infra
module T = Sxfiler_server_translator
module P = Procedure

(* defines procedure to get current key bindings *)
module Get_spec (G : G.Configuration.Get.S) : P.Spec = struct
  module Gateway = G

  let method_ = "configuration/get"
  let param_requirement = `Not_required ()
end
