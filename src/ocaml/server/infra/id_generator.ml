(** A implementation for {!module:Sxfiler_domain.Plan.Factory} signature. *)

module T = Sxfiler_domain

let id_gen = Uuidm.v4_gen (Random.get_state ())

module Gen_uuid : T.Id_generator_intf.Gen_random with type id = Uuidm.t = struct
  type id = Uuidm.t

  let generate () = id_gen ()
end

module Gen_random_string : T.Id_generator_intf.Gen_random with type id = string = struct
  type id = string

  let generate () = id_gen () |> Uuidm.to_string
end
