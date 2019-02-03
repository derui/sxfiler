(** Use cases for filer *)
open Sxfiler_core

module T = Sxfiler_domain

module Make_type = struct
  type input =
    { initial_location : Path.t
    ; name : string }

  type output = T.Filer.t
  type error = [`Already_exists]
end

(** {!Make_sync} module defines interface to make filer. *)
module type Make = sig
  (* trick to remove dependency for Make_type *)

  include module type of Make_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Make
    (CR : T.Configuration.Repository)
    (SR : T.Filer.Repository)
    (Svc : T.Location_scanner_service.S) : Make = struct
  include Make_type

  let execute (params : input) =
    (* Create filer if it is not exists *)
    let%lwt config = CR.resolve () in
    let%lwt v = SR.resolve params.name in
    match v with
    | None ->
      let sort_order = config.T.Configuration.default_sort_order in
      let%lwt nodes = Svc.scan params.initial_location in
      let t =
        T.Filer.make ~id:params.name ~location:params.initial_location ~nodes
          ~history:(T.Location_history.make ()) ~sort_order
      in
      let%lwt () = SR.store t in
      Lwt.return @@ Ok t
    | Some _ -> Lwt.return @@ Error `Already_exists
end

module Get_type = struct
  type input = {name : string}
  type output = T.Filer.t
  type error = [`Not_found]
end

module type Get = sig
  include module type of Get_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Get (SR : T.Filer.Repository) : Get = struct
  include Get_type

  let execute (params : input) =
    match%lwt SR.resolve params.name with
    | None -> Lwt.return_error `Not_found
    | Some filer -> Lwt.return_ok filer
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
      | Some filer ->
        let parent_dir = Path.dirname_as_path filer.T.Filer.location in
        let%lwt nodes = Svc.scan parent_dir in
        let filer' = T.Filer.move_location filer (module Clock) ~location:parent_dir ~nodes in
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
      (SR : T.Filer.Repository)
      (Svc : T.Location_scanner_service.S)
      (Clock : T.Location_record.Clock) : S = struct
    include Type

    let execute (params : input) =
      let%lwt filer = SR.resolve params.name in
      let node = Option.(filer >>= T.Filer.find_node ~id:params.node_id) in
      match (filer, node) with
      | None, _ -> Lwt.return_error `Not_found_filer
      | _, None -> Lwt.return_error `Not_found_node
      | Some filer, Some node ->
        if not node.stat.is_directory then Lwt.return_error `Not_directory
        else
          let%lwt new_nodes = Svc.scan node.full_path in
          let filer' =
            let location = node.full_path and nodes = new_nodes in
            T.Filer.move_location filer (module Clock) ~location ~nodes
          in
          let%lwt () = SR.store filer' in
          Lwt.return_ok filer'
  end
end

module Select_nodes = struct
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
      let filer = Option.(filer >>= lift @@ T.Filer.select_nodes ~ids:params.node_ids) in
      match filer with
      | None -> Lwt.return_error `Not_found
      | Some filer ->
        let%lwt () = SR.store filer in
        Lwt.return_ok filer
  end
end

module Deselect_nodes = struct
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
      let filer = Option.(filer >>= lift @@ T.Filer.deselect_nodes ~ids:params.node_ids) in
      match filer with
      | None -> Lwt.return_error `Not_found
      | Some filer ->
        let%lwt () = SR.store filer in
        Lwt.return_ok filer
  end
end

module Move_nodes = struct
  (* move nodes in filer to the location of another filer *)
  module Type = struct
    type t = A of int

    type input =
      { source_filer_id : T.Filer.id
      ; dest_filer_id : T.Filer.id
      ; node_ids : T.Node.id list
      ; corrections : T.Types.corrections }

    type output = unit
    type error = [`Not_found of T.Filer.id]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make
      (FR : T.Filer.Repository)
      (Scan : T.Location_scanner_service.S)
      (Transport : T.Node_transporter_service.S) : S = struct
    include Type

    let execute (params : input) =
      let%lwt source_filer = FR.resolve params.source_filer_id
      and dest_filer = FR.resolve params.dest_filer_id in
      match (source_filer, dest_filer) with
      | None, _ -> Lwt.return_error (`Not_found params.source_filer_id)
      | _, None -> Lwt.return_error (`Not_found params.dest_filer_id)
      | Some source_filer, Some dest_filer ->
        let subset, _ = T.Filer.node_subset source_filer ~ids:params.node_ids in
        let%lwt () =
          Transport.transport ~nodes:subset ~corrections:params.corrections ~_to:dest_filer
        in
        let%lwt from_nodes = Scan.scan source_filer.location
        and to_nodes = Scan.scan dest_filer.location in
        let from_filer = T.Filer.update_nodes source_filer ~nodes:from_nodes
        and to_filer = T.Filer.update_nodes dest_filer ~nodes:to_nodes in
        let%lwt () = Lwt.join [FR.store from_filer; FR.store to_filer] in
        Lwt.return_ok ()
  end
end

(** Use case to delete nodes *)
module Delete_nodes = struct
  (** Types for use case *)
  module Type = struct
    type input =
      { filer_id : T.Filer.id
      ; nodes : T.Node.id list }

    type output = unit
    type error = [`Not_found]
  end

  (** The signature of use case *)
  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  (** Make module with dependencies *)
  module Make
      (FR : T.Filer.Repository)
      (Scan : T.Location_scanner_service.S)
      (Trash : T.Node_trash_service.S) : S = struct
    include Type

    let execute (params : input) =
      let open Sxfiler_core.Option in
      match%lwt FR.resolve params.filer_id with
      | None -> Lwt.return_error `Not_found
      | Some filer ->
        let nodes =
          List.map (fun id -> T.Filer.(find_node filer ~id)) params.nodes
          |> List.filter Option.is_some |> List.map Option.get_exn
        in
        let%lwt () = Trash.trash nodes in
        let%lwt from_nodes = Scan.scan filer.location in
        let from_filer = T.Filer.update_nodes filer ~nodes:from_nodes in
        let%lwt () = FR.store from_filer in
        Lwt.return_ok ()
  end
end
