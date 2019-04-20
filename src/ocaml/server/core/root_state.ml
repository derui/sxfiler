module T = Sxfiler_domain

module String_map = Map.Make (struct
  type t = string

  let compare = Pervasives.compare
end)

type t =
  { configuration : T.Configuration.t
  ; filer_map : T.Filer.t String_map.t
  ; plan_map : T.Plan.t String_map.t }

let empty =
  { configuration = T.Configuration.default
  ; filer_map = String_map.empty
  ; plan_map = String_map.empty }

let find_filer ~id t = String_map.find_opt id t.filer_map
let add_filer ~filer t = {t with filer_map = String_map.add filer.T.Filer.id filer t.filer_map}
let add_plan ~plan t = {t with plan_map = String_map.add plan.T.Plan.id plan t.plan_map}
let remove_plan ~plan t = {t with plan_map = String_map.remove plan.T.Plan.id t.plan_map}
let find_plan ~id t = String_map.find_opt id t.plan_map
