(** State definitions for application. *)
module T = Sxfiler_types

module Viewer_stacks = struct
  type message = Message.t
  type t = Types.Viewer_stack.t Jstable.t

  (** [find_by_name t ~name] returns stack via [name] *)
  let find_by_name t ~name =
    Js.Optdef.to_option @@ Jstable.find t Js.(string name)

  let empty () = Jstable.create ()
  let update t = function
    | Message.Update_viewer_stack (name, viewer) ->
      let open Sxfiler_core in
      let stack = Option.get ~default:(Types.Viewer_stack.empty) @@ find_by_name t ~name in
      let stack = Types.(Viewer_stack.push stack ~v:viewer) in
      Jstable.add t Js.(string name) stack;
      t
end

module Config = struct
  type message = Message.t
  type t = T.Configuration.t

  let empty () = T.Configuration.default
  let update t _ = t
end
