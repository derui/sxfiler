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

(* move parent location from current location of filer *)
module Move_parent_type = struct
  type input = {name : string}
  type output = T.Filer.t
  type error = [`Not_found]
end

module type Move_parent = sig
  include module type of Move_parent_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Move_parent
    (SR : T.Filer.Repository)
    (Svc : T.Location_scanner_service.S)
    (Clock : T.Location_record.Clock) : Move_parent = struct
  include Move_parent_type

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

(* move to location of the node in filer *)
module Enter_directory_type = struct
  type input =
    { name : string
    ; node_id : string }

  type output = T.Filer.t

  type error =
    [ `Not_found_filer
    | `Not_found_node
    | `Not_directory ]
end

module type Enter_directory = sig
  include module type of Enter_directory_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Enter_directory
    (SR : T.Filer.Repository)
    (Svc : T.Location_scanner_service.S)
    (Clock : T.Location_record.Clock) : Enter_directory = struct
  include Enter_directory_type

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

(* move nodes in filer to the location of another filer *)
module Move_nodes_type = struct
  type input =
    { from : string
    ; node_ids : string list
    ; _to : string }

  type output = unit
  type error = [`Not_found_filer]
end

module type Move_nodes = sig
  include module type of Move_nodes_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Move_nodes
    (SR : T.Filer.Repository)
    (Scan : T.Location_scanner_service.S)
    (Transport : T.Node_transporter_service.S) : Move_nodes = struct
  include Move_nodes_type

  let execute (params : input) =
    let%lwt from_filer = SR.resolve params.from in
    let%lwt to_filer = SR.resolve params._to in
    match (from_filer, to_filer) with
    | None, _ | _, None -> Lwt.return_error `Not_found_filer
    | Some from_filer, Some to_filer ->
      let nodes =
        List.map (fun id -> T.Filer.find_node ~id from_filer) params.node_ids
        |> List.filter Option.is_some |> List.map Option.get_exn
      in
      let%lwt () = Transport.transport ~nodes ~_to:to_filer.T.Filer.location in
      let%lwt from_nodes = Scan.scan from_filer.location
      and to_nodes = Scan.scan to_filer.location in
      let from_filer = T.Filer.update_nodes from_filer ~nodes:from_nodes
      and to_filer = T.Filer.update_nodes to_filer ~nodes:to_nodes in
      let%lwt () = Lwt.join [SR.store from_filer; SR.store to_filer] in
      Lwt.return_ok ()
end
