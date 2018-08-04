open Sxfiler_domain_yojson

(** {!No_side_effect} module defines functions no side effect. What meaning of the word "side effect" in this module is
    to do not apply changes file system.
*)
module type No_side_effect = sig
  (** [resolve_realpath path] gets the absolute path of [directory].
      If [path] is relative, returns absolute path of current working directory by app.
  *)
  val resolve_realpath: string -> string

  (** [read_dir ~directory] take the list of nodes in [directory]. *)
  val read_dir: directory:string -> Node.t list Lwt.t
end

(** {!Side_effect} module defines functions having side effect. *)
module type Side_effect = sig
end

(** {!Instance} module gets instances of modules that defines functions. *)
module type Instance = sig
  module No_side_effect : No_side_effect
  module Side_effect : Side_effect
end
