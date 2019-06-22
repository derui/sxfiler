(** This module implements {!Sxfiler_domain.Notification_service.S} *)

module C = Sxfiler_server_core
include Notification_service_intf

module Make (CL : C.Rpc_client.S) : S = struct
  let send (type r) ~(typ : r typ) (v : r) =
    CL.notify
      ~api:{_method = typ.to_method v; params_to_json = typ.to_json; result_of_json = (fun _ -> ())}
      ~params:v ()
end
