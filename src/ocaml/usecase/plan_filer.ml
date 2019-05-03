(** This module provides use cases to make plan for filer  *)

open Sxfiler_core
module T = Sxfiler_domain

(** Make plan to move nodes between filers. *)
module Make_move_plan = struct
  (* Make plan to move nodes in filer to the location of another filer. *)
  module Type = struct
    type input =
      { source : T.Filer.id
      ; dest : T.Filer.id
      ; node_ids : T.Node.id list }

    type output = T.Plan.t

    type error =
      [ `Not_found of T.Filer.id
      | `Same_filer ]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  (** The executor of this plan. *)
  module Executor (P : sig
      val value : T.Filer.id * T.Filer.id
    end)
      (FR : T.Filer.Repository)
      (Transport : T.Node_transporter_service.S)
      (Scan : T.Location_scanner_service.S) : T.Plan.Executor = struct
    let do_plan target_nodes =
      let open Fun in
      let%lwt source_filer = FR.resolve & fst P.value and dest_filer = FR.resolve & snd P.value in
      match (source_filer, dest_filer) with
      | None, _ -> Lwt.return_error "Not found source filer"
      | _, None -> Lwt.return_error "Not found dest filer"
      | Some source_filer, Some dest_filer ->
        let transport node = function
          | T.Plan.Prediction.Fix (T.Plan.Correction.Rename new_name) ->
            Transport.transport ~node ~new_name ~_to:dest_filer.file_tree ()
          | Fix Overwrite | No_problem -> Transport.transport ~node ~_to:dest_filer.file_tree ()
          | Need_fix -> Lwt.fail_with "This branch can not reach"
        in
        let tasks =
          let open Option in
          target_nodes
          |> List.map (fun v ->
              T.Filer.find_node source_filer ~id:v.T.Plan.Target_node.node_id
              >|= fun node -> transport node v.T.Plan.Target_node.prediction )
          |> List.filter is_some |> List.map get_exn
        in
        let%lwt () = Lwt.join tasks in
        let%lwt from_tree = Scan.scan source_filer.file_tree.location
        and to_tree = Scan.scan dest_filer.file_tree.location in
        let from_filer = T.Filer.update_tree source_filer ~file_tree:from_tree
        and to_filer = T.Filer.update_tree dest_filer ~file_tree:to_tree in
        Lwt.(join [FR.store from_filer; FR.store to_filer] >>= return_ok)
  end

  module Make
      (FR : T.Filer.Repository)
      (PF : T.Plan.Factory.S)
      (Transport : T.Node_transporter_service.S)
      (Scan : T.Location_scanner_service.S) : S = struct
    include Type

    module Node_name_set = Set.Make (struct
        type t = string

        let compare = Stdlib.compare
      end)

    let execute (params : input) =
      let is_same_filer = params.source = params.dest in
      if is_same_filer then Lwt.return_error `Same_filer
      else
        let%lwt source = FR.resolve params.source and dest = FR.resolve params.dest in
        match (source, dest) with
        | None, _ -> Lwt.return_error (`Not_found params.source)
        | _, None -> Lwt.return_error (`Not_found params.dest)
        | Some source', Some dest' ->
          let node_name_set =
            List.fold_left
              (fun set node ->
                 let name = Path.basename node.T.Node.full_path in
                 Node_name_set.add name set )
              Node_name_set.empty dest'.file_tree.nodes
          in
          (* do prediction for target nodes *)
          let predict node =
            if Node_name_set.mem Path.(basename node.T.Node.full_path) node_name_set then
              T.Node.id node |> T.Plan.Target_node.need_fix
            else T.Node.id node |> T.Plan.Target_node.no_problem
          in
          let target_nodes =
            params.node_ids
            |> List.map (fun id -> Option.(T.Filer.find_node source' ~id >|= predict))
            |> List.filter Option.is_some |> List.map Option.get_exn
          in
          let module Executor =
            Executor (struct
              let value = (source'.id, dest'.id)
            end)
              (FR)
              (Transport)
              (Scan)
          in
          PF.create ~executor:(module Executor) ~target_nodes |> Lwt.return_ok
  end
end

(** Make plan to delete nodes *)
module Make_delete_plan = struct
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

  (** Use case to delete nodes *)
  module Executor (P : sig
      val value : T.Filer.id
    end)
      (FR : T.Filer.Repository)
      (Scan : T.Location_scanner_service.S)
      (Trash : T.Node_trash_service.S) : T.Plan.Executor = struct
    let do_plan target_nodes =
      let open Fun in
      match%lwt FR.resolve P.value with
      | None -> Lwt.return_error "Not found filer"
      | Some ({file_tree; _} as filer) ->
        let nodes =
          target_nodes
          |> List.map (fun v -> T.Filer.(find_node filer ~id:(T.Plan.Target_node.node_id v)))
          |> List.filter Option.is_some |> List.map Option.get_exn
        in
        let%lwt () = Trash.trash nodes in
        let%lwt file_tree = Scan.scan file_tree.T.File_tree.location in
        Lwt.((FR.store & T.Filer.update_tree filer ~file_tree) >>= return_ok)
  end

  module Make
      (FR : T.Filer.Repository)
      (PF : T.Plan.Factory.S)
      (Scan : T.Location_scanner_service.S)
      (Trash : T.Node_trash_service.S) : S = struct
    include Type

    let execute (params : input) =
      let%lwt source = FR.resolve params.source in
      match source with
      | None -> Lwt.return_error (`Not_found params.source)
      | Some source ->
        let target_nodes =
          params.node_ids
          |> List.map (fun id ->
              Option.(
                T.Filer.find_node source ~id
                >|= Fun.(T.Node.id %> T.Plan.Target_node.no_problem)) )
          |> List.filter Option.is_some |> List.map Option.get_exn
        in
        let module Executor =
          Executor (struct
            let value = source.id
          end)
            (FR)
            (Scan)
            (Trash)
        in
        PF.create ~executor:(module Executor) ~target_nodes |> Lwt.return_ok
  end
end

(** Make plan to copy nodes to other filer. *)
module Make_copy_plan = struct
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

  (** The executor of this plan. *)
  module Executor (P : sig
      val value : T.Filer.id * T.Filer.id
    end)
      (FR : T.Filer.Repository)
      (Replicate : T.Node_replication_service.S)
      (Scan : T.Location_scanner_service.S) : T.Plan.Executor = struct
    let do_plan target_nodes =
      let open Fun in
      let%lwt source_filer = FR.resolve & fst P.value and dest_filer = FR.resolve & snd P.value in
      match (source_filer, dest_filer) with
      | None, _ -> Lwt.return_error "Not found source filer"
      | _, None -> Lwt.return_error "Not found dest filer"
      | Some source_filer, Some dest_filer ->
        let transport node prediction =
          match prediction with
          | T.Plan.Prediction.Fix (T.Plan.Correction.Rename new_name) ->
            Replicate.replicate ~node ~new_name ~_to:dest_filer.file_tree ()
          | Fix Overwrite | No_problem -> Replicate.replicate ~node ~_to:dest_filer.file_tree ()
          | Need_fix -> Lwt.fail_with "This branch can not reach"
        in
        let tasks =
          let open Option in
          target_nodes
          |> List.map (fun v ->
              T.Filer.find_node source_filer ~id:v.T.Plan.Target_node.node_id
              >|= fun node -> transport node v.T.Plan.Target_node.prediction )
          |> List.filter is_some |> List.map get_exn
        in
        let%lwt () = Lwt.join tasks in
        let%lwt from_tree = Scan.scan source_filer.file_tree.location
        and to_tree = Scan.scan dest_filer.file_tree.location in
        let from_filer = T.Filer.update_tree source_filer ~file_tree:from_tree
        and to_filer = T.Filer.update_tree dest_filer ~file_tree:to_tree in
        Lwt.(join [FR.store from_filer; FR.store to_filer] >>= return_ok)
  end

  module Make
      (FR : T.Filer.Repository)
      (PF : T.Plan.Factory.S)
      (Replicate : T.Node_replication_service.S)
      (Scan : T.Location_scanner_service.S) : S = struct
    include Type

    let execute (params : input) =
      let%lwt source = FR.resolve params.source and dest = FR.resolve params.dest in
      match (source, dest) with
      | None, _ -> Lwt.return_error (`Not_found params.source)
      | _, None -> Lwt.return_error (`Not_found params.dest)
      | Some source', Some dest' ->
        let is_same_filer = source'.id = dest'.id in
        let target_nodes =
          params.node_ids
          |> List.map (fun id ->
              Option.(
                T.Filer.find_node source' ~id
                >|= Fun.(
                    if is_same_filer then T.Node.id %> T.Plan.Target_node.need_fix
                    else T.Node.id %> T.Plan.Target_node.no_problem)) )
          |> List.filter Option.is_some |> List.map Option.get_exn
        in
        let module Executor =
          Executor (struct
            let value = (source'.id, dest'.id)
          end)
            (FR)
            (Replicate)
            (Scan)
        in
        PF.create ~executor:(module Executor) ~target_nodes |> Lwt.return_ok
  end
end
