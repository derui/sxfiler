(** This module provides use cases to make plan for filer  *)

module T = Sxfiler_domain

(** Make plan to move nodes between filers. *)
module Move_nodes = struct
  (* Make plan to move nodes in filer to the location of another filer. *)
  module Type = struct
    type input = {workbench_id : T.Workbench.id}
    type output = T.Plan.t
    type error = [`Not_found_wb]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (WR : T.Workbench.Repository) : S = struct
    include Type

    let execute (params : input) =
      let%lwt wb = WR.resolve params.workbench_id in
      match wb with
      | None -> Lwt.return_error `Not_found_wb
      | Some wb ->
        let source = wb.env.source and dest = wb.env.dest and nodes = wb.env.nodes in
        let source_nodes =
          List.map
            (fun node ->
               if List.exists (fun v -> T.Node.(v.id = node.id)) nodes then
                 T.Plan.node_to_delete node
               else T.Plan.node_to_remain node )
            source.nodes
        and dest_nodes =
          List.concat
            [List.map T.Plan.node_to_remain dest.nodes; List.map T.Plan.node_to_append nodes]
        in
        T.Plan.make ~workbench_id:wb.id ~source:source_nodes ~dest:dest_nodes |> Lwt.return_ok
  end
end
