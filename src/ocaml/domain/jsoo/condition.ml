open Sxfiler_core
open Sxfiler_domain.Condition

class type js = object
  method onFileTree: bool Js.t Js.optdef Js.readonly_prop
  method onCompleting: bool Js.t Js.optdef Js.readonly_prop
end

let to_js t =
  let list = to_list t in
  object%js
    val onFileTree = Js.Optdef.return @@ Js.bool @@ List.mem On_file_tree list
    val onCompleting = Js.Optdef.return @@ Js.bool @@ List.mem On_completing list
  end

let of_js : js Js.t -> t = fun js ->
  let open Option.Infix in
  let use_context b = (Js.Optdef.to_option b >|= Js.to_bool) |> Option.get ~default:(fun () -> false) in
  List.fold_left (fun t (context, use_context) ->
      if use_context then enable ~context t else t
    ) empty
    [
      On_file_tree, use_context js##.onFileTree;
      On_completing, use_context js##.onCompleting;
    ]
