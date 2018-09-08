module T = Sxfiler_domain

module String_map = Map.Make (struct
    type t = string

    let compare = Pervasives.compare
  end)

type t =
  { configuration : T.Configuration.t
  ; filer_map : T.Filer.t String_map.t }

let empty = {configuration = T.Configuration.default; filer_map = String_map.empty}
let find_filer ~id t = String_map.find_opt id t.filer_map
let add_filer ~filer t = {t with filer_map = String_map.add filer.T.Filer.id filer t.filer_map}
