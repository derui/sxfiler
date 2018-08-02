(** Define actions are allowed user to call *)
open Sxfiler_core

module type Action = sig
  type t

  val module_name: string
  val to_string: t -> string
  val of_string: string -> t option
end

module Core = struct
  type t =
    | Change_current_workspace
    | Quit

  let module_name = "core"

  let to_string = function
    | Change_current_workspace -> "change_current_workspace"
    | Quit -> "quit"

  let of_string = function
    | _ as v when v = to_string Change_current_workspace ->
      Some Change_current_workspace
    | _ as v when v = to_string Quit->
      Some Quit
    | _ -> None
end

module File_list = struct
  type t =
      Next_item
    | Prev_item
    | Parent_directory
    | Enter_directory

  let module_name = "file_list"

  let to_string = function
    | Next_item -> "next_item"
    | Prev_item -> "prev_item"
    | Parent_directory -> "parent_directory"
    | Enter_directory -> "enter_directory"

  let of_string = function
    | _ as v when v = to_string Next_item -> Some Next_item
    | _ as v when v = to_string Prev_item -> Some Prev_item
    | _ as v when v = to_string Parent_directory -> Some Parent_directory
    | _ as v when v = to_string Enter_directory -> Some Enter_directory
    | _ -> None
end

module Completion = struct
  type t =
    | Next_candidate
    | Prev_candidate
    | Select_candidate

  let module_name = "completion"

  let to_string = function
    | Next_candidate -> "next_candidate"
    | Prev_candidate -> "prev_candidate"
    | Select_candidate -> "select_candidate"

  let of_string = function
    | _ as v when v = to_string Next_candidate -> Some Next_candidate
    | _ as v when v = to_string Prev_candidate -> Some Prev_candidate
    | _ as v when v = to_string Select_candidate -> Some Select_candidate
    | _ -> None
end

type t =
    Core of Core.t
  | File_list of File_list.t
  | Completion of Completion.t
  | Thrid_party of string * string

let action_of_string module_name action =
  let to_action (type t)
      (module C: Action with type t = t) action wrap =
    match C.of_string action with
    | Some v -> Some (wrap v)
    | None -> None
  in

  let action' = match module_name with
    | v when v = Core.module_name ->
      to_action (module Core) action (fun v -> Core v)
    | v when v = File_list.module_name ->
      to_action (module File_list) action (fun v -> File_list v)
    | _ -> Some (Thrid_party (module_name, action))
  in
  Option.get ~default:(fun () -> Thrid_party (module_name, action)) action'

let of_string action =
  let has_splitter = String.index_opt action ':' <> None in
  if has_splitter then begin
    let module_name = String.split_on_char ':' action in
    match module_name with
    | name :: [action] -> action_of_string name action
    | _ -> failwith (Printf.sprintf "Invalid format: %s" action)
  end
  else
    action_of_string Core.module_name action

let to_string = function
  | Core v -> Core.module_name ^ ":" ^ Core.to_string v
  | File_list v -> File_list.module_name ^ ":" ^ File_list.to_string v
  | Completion v -> Completion.module_name ^ ":" ^ Completion.to_string v
  | Thrid_party (name, action) -> name ^ ":" ^ action
