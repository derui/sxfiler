(** This module provides actor system via Websocket *)

include module type of struct
  include Ws_actor_intf
end

val make :
  (module Sxfiler_domain.Id_generator.S with type id = Uuidm.t) -> (module Ws_connection.Instance) -> (module Instance)
(** [make (module Id_generator) (module Conn)] make new instance of actor system *)
