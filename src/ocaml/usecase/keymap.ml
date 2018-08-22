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

module Type = struct
  type input = {
    context: string;
  }

  type output = string Key_map.t
end

module type Enable_context = Common.Usecase with type input = Type.input
                                             and type output = Type.output

(** This module defines rpc interface to enable context in this application. *)
module Enable_context
    (C:Condition.Repository)
    (R:Key_map_repository.S with type value = string): Enable_context = struct
  include Type

  let execute input =
    let%lwt cond = C.resolve () in
    let condition = Condition.enable cond ~context:input.context in
    let%lwt () = C.store condition in
    let%lwt keymap = R.resolve () in
    let keymap = Key_map.subset keymap ~condition in
    Lwt.return_ok keymap
end

module type Disable_context = Common.Usecase with type input = Type.input
                                             and type output = Type.output

(** This module defines rpc interface to disable context in this application. *)
module Disable_context
    (C:Condition.Repository)
    (R:Key_map_repository.S with type value = string): Disable_context = struct
  include Type

  let execute input =
    let%lwt cond = C.resolve () in
    let condition = Condition.disable cond ~context:input.context in
    let%lwt () = C.store condition in
    let%lwt keymap = R.resolve () in
    let keymap = Key_map.subset keymap ~condition in
    Lwt.return_ok keymap
end
