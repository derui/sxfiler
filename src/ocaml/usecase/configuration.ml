module T = Sxfiler_domain

module type Get = Common.Usecase with type input = unit
                                  and type output = T.Configuration.t
                                  and type error = unit

(** This module defines rpc interface to manage application configuration. *)
module Get(R:T.Configuration.Repository) : Get = struct
  type input = unit

  type output = T.Configuration.t
  type error = unit

  let execute () = let%lwt ret = R.resolve () in Lwt.return_ok ret
end

module type Store = Common.Usecase with type input = T.Configuration.t
                                    and type output = unit
                                    and type error = unit

(** This module defines rpc interface to store application configuration. *)
module Store(R:T.Configuration.Repository) : Store = struct
  type input = T.Configuration.t

  type output = unit
  type error = unit

  let execute input =
    let%lwt () = R.store input in
    Lwt.return_ok ()
end
