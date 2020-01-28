include module type of struct
  include Client_intf
end

open Sxfiler_infrastructure
open Sxfiler_domain

val make : (module Id_generator.S with type id = string) -> (module Ws_actor.Instance) -> (module Instance)
(** [make (module Connection)] create client with connection *)
