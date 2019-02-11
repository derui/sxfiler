(** This module provides use cases to make and reject plans. *)

open Sxfiler_core
module T = Sxfiler_domain

(** Execute the plan.  *)
module Execute = struct
  module Type = struct
    type input = {plan_id : T.Plan.id}
    type output = unit

    type error =
      [ `Not_found
      | `Need_fix
      | `Executor_error of string ]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (PR : T.Plan.Repository) : S = struct
    include Type

    let execute (params : input) =
      let open Fun in
      match%lwt PR.resolve params.plan_id with
      | None -> Lwt.return_error `Not_found
      | Some plan -> (
          let open Lwt in
          if not & T.Plan.is_all_target_allowed plan then Lwt.return_error `Need_fix
          else
            T.Plan.execute plan
            >>= function
            | Ok () -> PR.remove plan >>= return_ok | Error e -> return_error (`Executor_error e) )
  end
end

(** Reject the plan. *)
module Reject = struct
  module Type = struct
    type input = {plan_id : T.Plan.id}
    type output = unit
    type error = unit
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (PR : T.Plan.Repository) : S = struct
    include Type

    let execute (params : input) =
      match%lwt PR.resolve params.plan_id with
      | None -> Lwt.return_ok ()
      | Some plan -> Lwt.(PR.remove plan >>= Lwt.return_ok)
  end
end

(* Define aliases *)
module Filer = Plan_filer
