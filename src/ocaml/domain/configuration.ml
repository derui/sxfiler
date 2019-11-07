(** Configuration domain has all configurations of application. *)

type t = { default_sort_order : Types.Sort_type.t } [@@deriving show, eq]

let default = { default_sort_order = Types.Sort_type.Name }

module type Repository = sig
  val resolve : unit -> t Lwt.t
  (** [resolve ()] returns configuration. Configuration should be singleton. *)

  val store : t -> unit Lwt.t
  (** [store t] saves the [t] as singleton *)
end
