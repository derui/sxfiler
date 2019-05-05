(* Define use cases for task *)
open Sxfiler_domain

module Send_interaction = struct
  module Type = struct
    type input =
      { task_id : Task.id
      ; interaction : Task.interaction }

    type output = unit
    type error = [`Not_found]
  end

  module type S = sig
    (* trick to avoid error to unbound record field *)
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (R : Task.Repository) : S = struct
    include Type

    let execute {task_id; interaction} =
      match%lwt R.resolve task_id with
      | None -> Lwt.return_error `Not_found
      | Some t ->
          let open Lwt in
          Task.apply_interaction ~interaction t >>= return_ok
  end
end
