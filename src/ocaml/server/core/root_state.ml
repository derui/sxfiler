module T = Sxfiler_domain

module String_map = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

module Uuid_map = Map.Make (struct
  type t = Uuidm.t

  let compare = Uuidm.compare
end)

type t =
  { configuration : T.Configuration.t
  ; filer_map : T.Filer.t Uuid_map.t
  ; task_map : T.Task.t Uuid_map.t
  ; bookmark_map : T.Bookmark.t Uuid_map.t }

let empty =
  { configuration = T.Configuration.default
  ; filer_map = Uuid_map.empty
  ; task_map = Uuid_map.empty
  ; bookmark_map = Uuid_map.empty }

let find_filer ~id t =
  match Uuid_map.find_opt id t.filer_map with Some v -> v | None -> raise Not_found

let find_filer_by_name ~name t =
  Uuid_map.to_seq t.filer_map |> Seq.map snd |> List.of_seq
  |> List.find_opt (fun v -> v.T.Filer.name = name)

(** [list_filer t] get list of filer *)
let list_filer t = Uuid_map.to_seq t.filer_map |> Seq.map snd |> List.of_seq

let add_filer ~filer t = {t with filer_map = Uuid_map.add filer.T.Filer.id filer t.filer_map}
let find_task ~id t = Uuid_map.find_opt id t.task_map
let add_task ~task t = {t with task_map = Uuid_map.add task.T.Task.id task t.task_map}
let remove_task ~task t = {t with task_map = Uuid_map.remove task.T.Task.id t.task_map}
