(** Use cases for filer *)
open Sxfiler_core

module T = Sxfiler_domain

module Make = struct
  module Type = struct
    type input =
      { initial_location : Path.t
      ; name : string }

    type output = T.Filer.t
    type error = [`Already_exists]
  end

  (** {!Make_sync} module defines interface to make filer. *)
  module type S = sig
    (* trick to remove dependency for Make_type *)

    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (CR : T.Configuration.Repository)
      (SR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S) : S = struct
    include Type

    let execute (params : input) =
      (* Create filer if it is not exists *)
      let%lwt config = CR.resolve () in
      let%lwt v = SR.resolve params.name in
      match v with
      | None ->
          let sort_order = config.T.Configuration.default_sort_order in
          let%lwt file_tree = Svc.scan params.initial_location in
          let t =
            T.Filer.Factory.create ~name:params.name ~file_tree
              ~history:(T.Location_history.make ()) ~sort_order ()
          in
          let%lwt () = SR.store t in
          Lwt.return @@ Ok t
      | Some _ -> Lwt.return @@ Error `Already_exists
  end
end

module Get = struct
  module Type = struct
    type input = {name : string}
    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (SR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      match%lwt SR.resolve params.name with
      | None -> Lwt.return_error `Not_found
      | Some filer -> Lwt.return_ok filer
  end
end

module Move_parent = struct
  (* move parent location from current location of filer *)
  module Type = struct
    type input = {name : string}
    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (SR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S)
      (Clock : T.Location_record.Clock) : S = struct
    include Type

    let execute (params : input) =
      match%lwt SR.resolve params.name with
      | None -> Lwt.return_error `Not_found
      | Some ({file_tree; _} as filer) ->
          let parent_dir = Path.dirname_as_path file_tree.location in
          let%lwt file_tree = Svc.scan parent_dir in
          let filer' = T.Filer.move_location filer (module Clock) ~file_tree in
          let%lwt () = SR.store filer' in
          Lwt.return_ok filer'
  end
end

module Enter_directory = struct
  (* move to location of the node in filer *)
  module Type = struct
    type input =
      { name : string
      ; node_id : string }

    type output = T.Filer.t

    type error =
      [ `Not_found_filer
      | `Not_found_node
      | `Not_directory ]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (FR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S)
      (Clock : T.Location_record.Clock) : S = struct
    include Type

    let execute (params : input) =
      let%lwt filer = FR.resolve params.name in
      let node = Option.(filer >>= T.Filer.find_node ~id:params.node_id) in
      match (filer, node) with
      | None, _ -> Lwt.return_error `Not_found_filer
      | _, None -> Lwt.return_error `Not_found_node
      | Some filer, Some node ->
          if not node.stat.is_directory then Lwt.return_error `Not_directory
          else
            let%lwt new_file_tree = Svc.scan node.full_path in
            let filer' = T.Filer.move_location filer (module Clock) ~file_tree:new_file_tree in
            let%lwt () = FR.store filer' in
            Lwt.return_ok filer'
  end
end

module Toggle_mark = struct
  module Type = struct
    type input =
      { name : string
      ; node_ids : T.Node.id list }

    type output = T.Filer.t
    type error = [`Not_found]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (SR : T.Filer.Repository) : S = struct
    include Type

    let execute (params : input) =
      let%lwt filer = SR.resolve params.name in
      let filer =
        Option.(
          filer
          >>= lift
              @@ fun filer ->
              let marked, not_marked =
                List.partition
                  (fun v -> T.Filer.Node_id_set.mem v filer.T.Filer.marked_nodes)
                  params.node_ids
              in
              let filer = T.Filer.remove_mark filer ~ids:marked in
              T.Filer.add_mark filer ~ids:not_marked)
      in
      match filer with
      | None -> Lwt.return_error `Not_found
      | Some filer ->
          let%lwt () = SR.store filer in
          Lwt.return_ok filer
  end
end
