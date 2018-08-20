(** This module provides use cases for condition. *)
module T = Sxfiler_domain

module Type = struct
  type input = {
    context: string;
  }

  type output = unit
end

module type Enable = Common.Usecase with type input = Type.input
                                     and type output = Type.output

(** This module defines rpc interface to enable context in this application. *)
module Enable(R:T.Condition.Repository) : Enable = struct
  include Type

  let execute input =
    let%lwt cond = R.resolve () in
    let condition = T.Condition.enable cond ~context:input.context in
    let%lwt () = R.store condition in
    Lwt.return_ok ()
end

module type Disable = Common.Usecase with type input = Type.input
                                      and type output = Type.output

(** This module defines rpc interface to disable context in this application. *)
module Disable(R:T.Condition.Repository) : Disable = struct
  include Type

  let execute input =
    let%lwt cond = R.resolve () in
    let condition = T.Condition.disable cond ~context:input.context in
    let%lwt () = R.store condition in
    Lwt.return_ok ()
end
