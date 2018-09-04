(** Signature for executor. *)
module type S = sig
  val run : Workbench.t -> unit Lwt.t
  (** [run workbench] execute the work implemented on [workbench].
      Execute will ignores plan.
  *)
end
