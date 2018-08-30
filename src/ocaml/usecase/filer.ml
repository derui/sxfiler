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

module Make (CR : T.Configuration.Repository) (SR : T.Filer.Repository) (NR : T.Node.Repository) :
  Make = struct
  include Make_type

  let execute (params : input) =
    (* Create filer if it is not exists *)
    let%lwt config = CR.resolve () in
    let%lwt v = SR.resolve params.name in
    match v with
    | None ->
      let sort_order = config.T.Configuration.default_sort_order in
      let%lwt nodes = NR.find_by_dir ~dir:params.initial_location in
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
    (NR : T.Node.Repository)
    (Clock : T.Location_record.Clock) : Move_parent = struct
  include Move_parent_type

  let execute (params : input) =
    match%lwt SR.resolve params.name with
    | None -> Lwt.return_error `Not_found
    | Some filer ->
      let parent_dir = Path.dirname_as_path filer.T.Filer.location in
      let%lwt new_nodes = NR.find_by_dir ~dir:parent_dir in
      let filer' =
        T.Filer.move_location filer (module Clock) ~location:parent_dir ~nodes:new_nodes
      in
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
    (NR : T.Node.Repository)
    (Clock : T.Location_record.Clock) : Enter_directory = struct
  include Enter_directory_type

  let execute (params : input) =
    let%lwt filer = SR.resolve params.name in
    let node = Option.Infix.(filer >>= T.Filer.find_node ~id:params.node_id) in
    match (filer, node) with
    | None, _ -> Lwt.return_error `Not_found_filer
    | _, None -> Lwt.return_error `Not_found_node
    | Some filer, Some node ->
      if not node.stat.is_directory then Lwt.return_error `Not_directory
      else
        let%lwt new_nodes = NR.find_by_dir ~dir:node.full_path in
        let filer' =
          let location = node.full_path and nodes = new_nodes in
          T.Filer.move_location filer (module Clock) ~location ~nodes
        in
        let%lwt () = SR.store filer' in
        Lwt.return_ok filer'
end
