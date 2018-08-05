open Sxfiler_domain

module type Get = Common.Usecase with type input = unit
                                  and type output = string Key_map.t

(** This module defines usecase interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get(R:Key_map_repository.S with type value = string) : Get = struct
  type input = unit

  type output = string Key_map.t

  let execute () = let open Lwt in R.resolve () >>= return_ok
end

module type Store = Common.Usecase with type input = string Key_map.t
                                    and type output = unit

(** This module defines usecase interface to store key map with repository *)
module Store(R:Key_map_repository.S with type value = string) : Store = struct
  type input = string Key_map.t

  type output = unit

  let execute input = let open Lwt in R.store input >>= return_ok
end
