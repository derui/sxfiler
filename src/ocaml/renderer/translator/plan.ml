(** Plan provides translators between JSON and OCaml representation *)
open Sxfiler_core

module P = Sxfiler_rpc.Types.Plan

class type node_plan =
  object
    method operation : int Js.readonly_prop

    method node : Node.js Js.t Js.readonly_prop
  end

class type js =
  object
    method source : node_plan Js.t Js.js_array Js.t Js.readonly_prop

    method dest : node_plan Js.t Js.js_array Js.t Js.readonly_prop
  end

let node_plan_of_js js : P.node_plan =
  let open Fun in
  {operation = (P.Operation.of_int %> Option.get_exn) js##.operation; node = Node.of_js js##.node}

let node_plan_to_js t : node_plan Js.t =
  object%js
    val operation = P.Operation.to_int t.P.operation

    val node = Node.to_js t.node
  end

let of_js js : P.t =
  (* full_path should be absolute path. *)
  let conv = Fun.(Js.array_map node_plan_of_js %> Js.to_array %> Array.to_list) in
  {source = conv js##.source; dest = conv js##.dest}

let to_js t : js Js.t =
  let conv = Fun.(List.map node_plan_to_js %> Array.of_list %> Js.array) in
  object%js
    val source = conv t.P.source

    val dest = conv t.dest
  end
