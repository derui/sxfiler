(** Provides RPC client and server implementation. This implementation only depends on
    Jsonrpc_ocaml_jsoo's interface. All modules are purely.
*)
include Rpc_server_intf

(** {!Server} provides some functions to handle notification from server. *)
module Core : S = struct
  module Response = R.Response
  module Request = R.Request
  module Thread = Lwt

  type handler = R.Request.t -> R.Response.t Lwt.t
  type _method = string
  type t = {procedure_table : handler Jstable.t}

  let make () = {procedure_table = Jstable.create ()}

  let expose ~_method ~handler t =
    let _method' = Js.string _method in
    Jstable.add t.procedure_table _method' handler ;
    t

  let unexpose ~_method t =
    Jstable.remove t.procedure_table Js.(string _method) ;
    t

  let handle_request ~request t =
    let open Jsonrpc_ocaml_jsoo in
    match
      Jstable.find t.procedure_table Js.(string request.Request._method) |> Js.Optdef.to_option
    with
    | None -> (* TODO: logging unknown notification *)
      Lwt.return Response.empty
    | Some handler -> Lwt.catch (fun () -> handler request) (fun e -> raise e)
end

include Core
