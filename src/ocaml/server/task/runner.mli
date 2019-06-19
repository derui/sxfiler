include module type of struct
  include Runner_intf
end

val make :
  (module Sxfiler_domain.Id_generator_intf.Gen_random with type id = Uuidm.t) -> (module Instance)
(** [make (module Gen)] get the new instance of runner. *)
