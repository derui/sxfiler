(** Rpc_connection handle request and response on Websocket as Lwt stream.
    This module defines global connection to be able to use other module to send
    frame.
*)
include module type of struct
  include Rpc_connection_intf
end

val make : unit -> (module Instance)
(** [make ()] makes new connection that is default implementation. *)
