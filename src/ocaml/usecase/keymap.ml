open Sxfiler_domain

(** Module to share interface and structure. *)
module Get_type = struct
  type input = unit
  type output = string Key_map.t
  type error = unit
end

module type Get =
  Common.Usecase
  with type input = Get_type.input
   and type output = Get_type.output
   and type error = Get_type.error

(** This module defines usecase interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get (C : Condition.Repository) (R : Key_map_repository.S with type value = string) : Get =
struct
  include Get_type

  let execute () =
    let%lwt condition = C.resolve () in
    let%lwt keymap = R.resolve () in
    let keymap = Key_map.subset keymap ~condition in
    Lwt.return_ok keymap
end

(** Module to share interface and structure. *)
module Store_type = struct
  type input = string Key_map.t
  type output = unit
  type error = unit
end

module type Store =
  Common.Usecase
  with type input = Store_type.input
   and type output = Store_type.output
   and type error = Store_type.error

(** This module defines usecase interface to store key map with repository *)
module Store (R : Key_map_repository.S with type value = string) : Store = struct
  include Store_type

  let execute input =
    let open Lwt in
    R.store input >>= return_ok
end

(** Module to share interface and structure. *)
module Context_type = struct
  type input = {context : string}
  type output = string Key_map.t
  type error = unit
end

module type Enable_context =
  Common.Usecase
  with type input = Context_type.input
   and type output = Context_type.output
   and type error = Context_type.error

(** This module defines rpc interface to enable context in this application. *)
module Enable_context
    (C : Condition.Repository)
    (R : Key_map_repository.S with type value = string) : Enable_context = struct
  include Context_type

  let execute input =
    let%lwt cond = C.resolve () in
    let condition = Condition.enable cond ~context:input.context in
    let%lwt () = C.store condition in
    let%lwt keymap = R.resolve () in
    let keymap = Key_map.subset keymap ~condition in
    Lwt.return_ok keymap
end

module type Disable_context =
  Common.Usecase
  with type input = Context_type.input
   and type output = Context_type.output
   and type error = Context_type.error

(** This module defines rpc interface to disable context in this application. *)
module Disable_context
    (C : Condition.Repository)
    (R : Key_map_repository.S with type value = string) : Disable_context = struct
  include Context_type

  let execute input =
    let%lwt cond = C.resolve () in
    let condition = Condition.disable cond ~context:input.context in
    let%lwt () = C.store condition in
    let%lwt keymap = R.resolve () in
    let keymap = Key_map.subset keymap ~condition in
    Lwt.return_ok keymap
end
