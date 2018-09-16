(** This module provides use cases to make and reject plans. *)

module T = Sxfiler_domain

(** Reject the plan. *)
module Reject = struct
  module Type = struct
    type input = {workbench_id : T.Workbench.id}
    type output = unit
    type error = unit
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
      match wb with None -> Lwt.return_ok () | Some wb -> Lwt.(WR.remove wb >>= Lwt.return_ok)
  end
end

(* Define aliases *)
module Filer = Plan_filer
