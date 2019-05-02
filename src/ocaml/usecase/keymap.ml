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

  (** This module defines usecase interface to get current key bindings.
      Replace [json] on implementation to match rpc.
  *)
  module Make (C : Condition.Repository) (R : Key_map_repository.S) : S = struct
    include Type

    let execute () =
      let%lwt condition = C.resolve () in
      let%lwt keymap = R.resolve () in
      let keymap = Key_map.subset keymap ~condition in
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
      (CR : Configuration.Repository)
      (R : Key_map_repository.S)
      (S : Key_map_resolve_service.S) : S = struct
    include Type

    let execute () =
      let%lwt config = CR.resolve () in
      let%lwt key_map = S.resolve config.key_map_file in
      let%lwt () = R.store key_map in
      let%lwt condition = CNDR.resolve () in
      let key_map = Key_map.subset key_map ~condition in
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

(** Module to share interface and structure. *)
module Context_type = struct
  type input = {context : string}
  type output = Key_map.t
  type error = unit
end

module Add_context = struct
  module type S =
    Common.Usecase
    with type input = Context_type.input
     and type output = Context_type.output
     and type error = Context_type.error

  (** This module defines rpc interface to enable context in this application. *)
  module Make (C : Condition.Repository) (R : Key_map_repository.S) : S = struct
    include Context_type

    let execute input =
      let%lwt cond = C.resolve () in
      let condition = Condition.enable cond ~context:input.context in
      let%lwt () = C.store condition in
      let%lwt keymap = R.resolve () in
      let keymap = Key_map.subset keymap ~condition in
      Lwt.return_ok keymap
  end
end

module Delete_context = struct
  module type S =
    Common.Usecase
    with type input = Context_type.input
     and type output = Context_type.output
     and type error = Context_type.error

  (** This module defines rpc interface to disable context in this application. *)
  module Make (C : Condition.Repository) (R : Key_map_repository.S) : S = struct
    include Context_type

    let execute input =
      let%lwt cond = C.resolve () in
      let condition = Condition.disable cond ~context:input.context in
      let%lwt () = C.store condition in
      let%lwt keymap = R.resolve () in
      let keymap = Key_map.subset keymap ~condition in
      Lwt.return_ok keymap
  end
end
