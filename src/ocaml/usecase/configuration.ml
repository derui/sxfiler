module T = Sxfiler_domain

module Get_type = struct
  type input = unit
  type output = T.Configuration.t
  type error = unit
end

module type Get =
  Common.Usecase
  with type input = Get_type.input
   and type output = Get_type.output
   and type error = Get_type.error

(** This module defines rpc interface to manage application configuration. *)
module Get (R : T.Configuration.Repository) : Get = struct
  include Get_type

  let execute () =
    let%lwt ret = R.resolve () in
    Lwt.return_ok ret
end

module Store_type = struct
  type input = T.Configuration.t
  type output = unit
  type error = unit
end

module type Store =
  Common.Usecase
  with type input = Store_type.input
   and type output = Store_type.output
   and type error = Store_type.error

(** This module defines rpc interface to store application configuration. *)
module Store (R : T.Configuration.Repository) : Store = struct
  include Store_type

  let execute input =
    let%lwt () = R.store input in
    Lwt.return_ok ()
end
