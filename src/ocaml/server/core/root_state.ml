module T = Sxfiler_domain

module String_map = Map.Make (struct
    type t = string

    let compare = Pervasives.compare
  end)

module Uuid_map = Map.Make (struct
    type t = Uuidm.t

    let compare = Uuidm.compare
  end)

type t =
  { configuration : T.Configuration.t
  ; filer_map : T.Filer.t String_map.t
  ; task_map : T.Task.t Uuid_map.t }

let empty =
  {configuration = T.Configuration.default; filer_map = String_map.empty; task_map = Uuid_map.empty}

let find_filer ~id t = String_map.find_opt id t.filer_map
let add_filer ~filer t = {t with filer_map = String_map.add filer.T.Filer.id filer t.filer_map}
let find_task ~id t = Uuid_map.find_opt id t.task_map
let add_task ~task t = {t with task_map = Uuid_map.add task.T.Task.id task t.task_map}
let remove_task ~task t = {t with task_map = Uuid_map.remove task.T.Task.id t.task_map}

(** set key map file *)
let set_key_map_path ~path t =
  {t with configuration = T.Configuration.{t.configuration with key_map_file = path}}
