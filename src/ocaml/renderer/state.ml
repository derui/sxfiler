(** State definitions for application. *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

module Viewer_stacks = struct
  type message = C.Message.t
  type t = C.Types.Viewer_stack.t Jstable.t

  (** [find_by_name t ~name] returns stack via [name] *)
  let find_by_name t ~name =
    Js.Optdef.to_option @@ Jstable.find t Js.(string name)


      (* let store = C.Store_group.get store_group ~tag:Store.viewer_stacks in
       * let module S = Store.Viewer_stacks_store in
       *
       * let viewer = C.Types.(Viewer_state.make @@ Viewer.(File_tree {
       *     File_tree.scanner = params.scanner;
       *     selected_item_index = 0;
       *
       *   })) in *)
  let empty () = Jstable.create ()
  let reduce t = function
    | C.Message.Update_viewer_stack (name, viewer) ->
      let open Sxfiler_core in
      let stack = Option.get ~default:(C.Types.Viewer_stack.empty) @@ find_by_name t ~name in
      let stack = C.Types.(Viewer_stack.push stack ~v:viewer) in
      Jstable.add t Js.(string name) stack;
      t
end

module Config = struct
  type message = C.Message.t
  type t = T.Configuration.t

  let empty () = T.Configuration.default
  let reduce t _ = t
end

module Layout = struct
  type message = C.Message.t
  type t = string list

  let empty () = [Const.workspace_1;Const.workspace_2]
  let reduce t _ = t
end
