(** This module provides use cases to manage plan *)
open Sxfiler_core

module T = Sxfiler_domain

(* Make plan to with some planner. *)
module Make_type = struct
  type input =
    { from : T.Filer.id
    ; node_ids : string list
    ; _to : T.Filer.id }

  type output = T.Workbench.t
  type error = [`Not_found_filer]
end

module type Make = sig
  include module type of Make_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Make (SR : T.Filer.Repository) (Factory : T.Workbench.Factory) (WR : T.Workbench.Repository) :
  Make = struct
  include Make_type

  let execute (params : input) =
    let%lwt from_filer = SR.resolve params.from in
    let%lwt to_filer = SR.resolve params._to in
    match (from_filer, to_filer) with
    | None, _ | _, None -> Lwt.return_error `Not_found_filer
    | Some source, Some dest ->
      let nodes =
        List.map (fun id -> T.Filer.find_node ~id source) params.node_ids
        |> List.filter Option.is_some |> List.map Option.get_exn
      in
      let env = {T.Workbench.source; dest; nodes} in
      let wb = Factory.make env in
      let%lwt () = WR.store wb in
      Lwt.return_ok wb
end

(* Clean up workbench. *)
module Cleanup_type = struct
  type input = {workbench_id : T.Workbench.id}
  type output = unit
  type error = [`Not_found]
end

module type Cleanup = sig
  include module type of Cleanup_type
  include Common.Usecase with type input := input and type output := output and type error := error
end

module Cleanup (WR : T.Workbench.Repository) : Cleanup = struct
  include Cleanup_type

  let execute (params : input) =
    let%lwt wb = WR.resolve params.workbench_id in
    match wb with
    | None -> Lwt.return_error `Not_found
    | Some wb -> Lwt.(WR.remove wb >>= Lwt.return_ok)
end