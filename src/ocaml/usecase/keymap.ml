open Sxfiler_domain

module Get = struct
  (** Module to share interface and structure. *)
  module Type = struct
    type input = unit
    type output = Key_map.t
    type error = unit
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  (** This module defines usecase interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (C : Condition.Repository) (R : Key_map_repository.S) : S = struct
    include Type

    let execute () =
      let%lwt keymap = R.resolve () in
      Lwt.return_ok keymap
  end
end

module Reload = struct
  (** Module to share interface and structure. *)
  module Type = struct
    type input = unit
    type output = Key_map.t
    type error = unit
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  (** This module defines usecase interface to store key map with repository *)
  module Make
      (CNDR : Condition.Repository)
      (R : Key_map_repository.S)
      (S : Key_map_resolve_service.S) : S = struct
    include Type

    let execute () =
      let%lwt key_map = S.resolve () in
      let%lwt () = R.store key_map in
      Lwt.return_ok key_map
  end
end

module Store = struct
  (** Module to share interface and structure. *)
  module Type = struct
    type input = Key_map.t
    type output = unit
    type error = unit
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  (** This module defines usecase interface to store key map with repository *)
  module Make (R : Key_map_repository.S) : S = struct
    include Type

    let execute input =
      let open Lwt in
      R.store input >>= return_ok
  end
end
