module T = Sxfiler_domain

module Get = struct
  module Type = struct
    type input = unit
    type output = T.Configuration.t
    type error = unit
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  (** This module defines rpc interface to manage application configuration. *)
  module Make (R : T.Configuration.Repository) : S = struct
    include Type

    let execute () =
      let%lwt ret = R.resolve () in
      Lwt.return_ok ret
  end
end

module Store = struct
  module Type = struct
    type input = T.Configuration.t
    type output = unit
    type error = unit
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  (** This module defines rpc interface to store application configuration. *)
  module Make (R : T.Configuration.Repository) : S = struct
    include Type

    let execute input =
      let%lwt () = R.store input in
      Lwt.return_ok ()
  end
end
