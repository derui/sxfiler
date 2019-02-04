(** This module provides use cases to make plan for filer  *)

open Sxfiler_core
module T = Sxfiler_domain

(** Make plan to move nodes between filers. *)
module Move_nodes = struct
  (* Make plan to move nodes in filer to the location of another filer. *)
  module Type = struct
    type input =
      { source : T.Filer.id
      ; dest : T.Filer.id
      ; node_ids : T.Node.id list }

    type output = T.Plan.t
    type error = [`Not_found of T.Filer.id]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (FR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      let%lwt source = FR.resolve params.source and dest = FR.resolve params.dest in
      match (source, dest) with
      | None, _ -> Lwt.return_error (`Not_found params.source)
      | _, None -> Lwt.return_error (`Not_found params.dest)
      | Some source, Some dest ->
        let target_nodes, _ = T.Filer.node_subset source ~ids:params.node_ids in
        let is_same_filer = Path.to_string source.location = Path.to_string dest.location in
        if is_same_filer then T.Plan.make ~source ~dest ~plans:[] |> Lwt.return_ok
        else
          let plans = List.map T.Plan.plan_move target_nodes in
          T.Plan.make ~source ~dest ~plans |> Lwt.return_ok
  end
end

(** Make plan to delete nodes *)
module Delete_nodes = struct
  module Type = struct
    type input =
      { source : T.Filer.id
      ; node_ids : T.Node.id list }

    type output = T.Plan.t
    type error = [`Not_found of T.Filer.id]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (FR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      let%lwt source = FR.resolve params.source in
      match source with
      | None -> Lwt.return_error (`Not_found params.source)
      | Some source ->
        let target_nodes, _ = T.Filer.node_subset source ~ids:params.node_ids in
        let plans = List.map T.Plan.plan_delete target_nodes in
        T.Plan.make ~source ~dest:source ~plans |> Lwt.return_ok
  end
end

(** Make plan to copy nodes to other filer. *)
module Copy_nodes = struct
  (* Make plan to move nodes in filer to the location of another filer. *)
  module Type = struct
    type input =
      { source : T.Filer.id
      ; dest : T.Filer.id
      ; node_ids : T.Node.id list }

    type output = T.Plan.t
    type error = [`Not_found of T.Filer.id]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (FR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      let%lwt source = FR.resolve params.source and dest = FR.resolve params.dest in
      match (source, dest) with
      | None, _ -> Lwt.return_error (`Not_found params.source)
      | _, None -> Lwt.return_error (`Not_found params.dest)
      | Some source, Some dest ->
        let target_nodes, _ = T.Filer.node_subset source ~ids:params.node_ids in
        let is_same_filer = Path.to_string source.location = Path.to_string dest.location in
        if is_same_filer then T.Plan.make ~source ~dest ~plans:[] |> Lwt.return_ok
        else
          let plans = List.map T.Plan.plan_copy target_nodes in
          T.Plan.make ~source ~dest ~plans |> Lwt.return_ok
  end
end
