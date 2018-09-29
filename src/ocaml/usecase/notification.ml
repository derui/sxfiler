(** This module provides use cases to send notification. *)

module T = Sxfiler_domain

(** Notify a object. *)
module Notify = struct
  module Type = struct
    type input =
      { notification : T.Notification.body
      ; level : T.Notification.Level.t }

    type output = unit
    type error = unit
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase with type input := input and type output := output and type error := error
  end

  module Make (NF : T.Notification.Factory) (NS : T.Notification_service.S) : S = struct
    include Type

    let execute (params : input) =
      let notification = NF.create ~level:params.level ~body:params.notification in
      let open Lwt in
      NS.send notification >>= return_ok
  end
end
