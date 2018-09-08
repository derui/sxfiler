(** Backend for workbench *)
module D = Sxfiler_domain

module Uuid_map = Map.Make (struct
    type t = Uuidm.t

    let compare = Uuidm.compare
  end)

type t = D.Workbench.t Uuid_map.t

let empty = Uuid_map.empty
let add ~value = Uuid_map.add value.D.Workbench.id value
let remove ~id = Uuid_map.remove id
let find ~id = Uuid_map.find_opt id
