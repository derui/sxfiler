open Sxfiler_core

(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type t =
    | Command
    | Completion
    | File_tree

end

(* Condition defines condition to handle timing to enable key binding.*)
module Condition = struct
  type context =
    | On_file_tree
    | On_completing

  module Context_set = Set.Make(struct
      type t = context
      let compare = Pervasives.compare
    end)

  type t = Context_set.t

  class type js = object
    method onFileTree: bool Js.t Js.optdef Js.readonly_prop
    method onCompleting: bool Js.t Js.optdef Js.readonly_prop
  end

  let equal = Context_set.equal

  let empty = Context_set.empty
  let of_list contexts = List.fold_left (fun set context -> Context_set.add context set)
      empty contexts

  let enable t ~context = Context_set.add context t
  let disable t ~context = Context_set.remove context t

  let to_js t = object%js
    val onFileTree = Js.Optdef.return @@ Js.bool @@ Context_set.mem On_file_tree t
    val onCompleting = Js.Optdef.return @@ Js.bool @@ Context_set.mem On_completing t
  end

  let of_js : js Js.t -> t = fun js ->
    let open Option.Infix in
    let use_context b = (Js.Optdef.to_option b >|= Js.to_bool) |> Option.get ~default:(fun () -> false) in
    List.fold_left (fun set (context, use_context) ->
        if use_context then Context_set.add context set
        else set
      ) Context_set.empty
      [
        On_file_tree, use_context js##.onFileTree;
        On_completing, use_context js##.onCompleting;
      ]

  let subset ~current ~parts = Context_set.subset parts current
end
