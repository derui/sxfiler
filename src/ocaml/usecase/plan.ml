(** This module provides use cases to make and reject plans. *)

module T = Sxfiler_domain

module type Executer = sig
  type param

  val execute : param -> T.Plan.Target_node.t list -> unit Lwt.t
end

module type Executer_hub = sig
  module Move : Executer with type param = T.Filer.id * T.Filer.id
  module Copy : Executer with type param = T.Filer.id * T.Filer.id
  module Delete : Executer with type param = T.Filer.id
  module Change_mode : Executer with type param = T.Filer.id
end

module Dispatcher = struct
  module type S = sig
    val execute : T.Plan.t -> unit Lwt.t
  end

  module Make (H : Executer_hub) : S = struct
    let execute {T.Plan.plan_type; target_nodes; _} =
      match plan_type with
      | T.Plan.Type.Move (f1, f2) -> H.Move.execute (f1, f2) target_nodes
      | Copy (f1, f2) -> H.Copy.execute (f1, f2) target_nodes
      | Delete v -> H.Delete.execute v target_nodes
      | Change_mode v -> H.Change_mode.execute v target_nodes
  end
end

(** Execute the plan.  *)
module Execute = struct
  module Type = struct
    type input = {plan_id : T.Plan.id}
    type output = unit

    type error =
      [ `Not_found
      | `Need_fix ]
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (PR : T.Plan.Repository) (E : Executer) : S = struct
    include Type

    let execute (params : input) =
      match%lwt PR.resolve params.plan_id with
      | None -> Lwt.return_error `Not_found
      | Some plan ->
        let open Lwt in
        E.execute plan >>= fun () -> PR.remove plan >>= return_ok
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
