(** Define actions are allowed user to call *)
module Core = struct
  type t =
    | Change_current_workspace
    | Quit
    | Unknown of string

  let module_name = "core"

  let to_string = function
    | Change_current_workspace -> "change_current_workspace"
    | Quit -> "quit"
    | Unknown v -> v

  let of_string = function
    | "change_current_workspace" -> Change_current_workspace
    | "quit" -> Quit
    | _ as v -> Unknown v
end

module File_list = struct
  type t =
      Next_item
    | Prev_item
    | Parent_directory
    | Enter_directory
    | Unknown of string

  let module_name = "file_list"

  let to_string = function
    | Next_item -> "next_item"
    | Prev_item -> "prev_item"
    | Parent_directory -> "parent_directory"
    | Enter_directory -> "enter_directory"
    | Unknown v -> v

  let of_string = function
    | "next_item" -> Next_item
    | "prev_item" -> Prev_item
    | "parent_directory" -> Parent_directory
    | "enter_directory" -> Enter_directory
    | _ as v -> Unknown v
end

type t =
    Core of Core.t
  | File_list of File_list.t
  | Thrid_party of string * string

let action_of_string module_name action =
  match module_name with
  | v when v = Core.module_name -> Core (Core.of_string action)
  | _ -> Thrid_party (module_name, action)

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
  | Thrid_party (name, action) -> name ^ ":" ^ action
