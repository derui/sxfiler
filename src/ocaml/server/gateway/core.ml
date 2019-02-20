(** This signature defines the signature of base gateway module. *)
module type Gateway = sig
  type params [@@deriving of_yojson]
  type result [@@deriving to_yojson]

  val handle : params -> result Lwt.t
end
