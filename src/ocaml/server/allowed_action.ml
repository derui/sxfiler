(** This module defines functions are allowed to use in tasks.  *)
module T = Sxfiler_types_yojson

(** {!No_side_effect} module defines functions no side effect. What meaning of the word "side effect" in this module is
    to do not apply changes file system.
*)
module type No_side_effect = sig
  (** [take_snapshot ~directory] take the snapshot of [directory]. *)
  val take_snapshot: directory:string -> T.Tree_snapshot.t Lwt.t
end

(** {!Side_effect} module defines functions having side effect. *)
module type Side_effect = sig
end

(** {!Instance} module gets instances of modules that defines functions. *)
module type Instance = sig
  module No_side_effect : No_side_effect
  module Side_effect : Side_effect
end

module Dummy : Instance = struct
  module No_side_effect : No_side_effect = struct
    let take_snapshot ~directory = Lwt.return @@ T.Tree_snapshot.make ~directory ~nodes:[]
  end

  module Side_effect : Side_effect = struct end
end
