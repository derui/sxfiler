open Sxfiler_core
module Ty = Sxfiler_types

(* Condition defines condition to handle timing to enable key binding.*)
module Condition = struct
  type t = {
    on_file_tree: bool;
  }

  class type js = object
    method onFileTree: bool Js.t Js.optdef Js.readonly_prop
  end

  let empty = {
    on_file_tree = false;
  }

  let to_js t = object%js
    val onFileTree = Js.Optdef.return @@ Js.bool t.on_file_tree
  end

  let of_js : js Js.t -> t = fun js ->
    let open Option.Infix in
    {
      on_file_tree = (Js.Optdef.to_option js##.onFileTree >|= Js.to_bool) |> Option.get ~default:(fun () -> false);
    }
end

(** {!Mode} defines type that is current mode of view. *)
module Mode = struct
  type t =
    | Command
    | Completion
    | File_tree

end
