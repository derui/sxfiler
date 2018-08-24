(** Configuration domain has all configurations of application. *)

type t = {
  default_sort_order: Types.Sort_type.t;
}

let default = {
  default_sort_order = Types.Sort_type.Name;
}

module type Repository = sig
  (** [resolve ()] returns configuration. Configuration should be singleton.  *)
  val resolve: unit -> t Lwt.t

  (** [store t] saves the [t] as singleton *)
  val store: t -> unit Lwt.t
end
