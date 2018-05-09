(** Define actions are allowed user to call *)
module Core = struct
  type t =
      Next_item
    | Prev_item
    | Leave_directory
    | Enter_directory
    | Change_active_pane
    | Copy
    | Move
    | Delete
    | Rename
    | Jump
    | Quit
    | Toggle_mark
    | Make_dir
    | History
    | Change_permission
    | Toggle_bookmark
    | Unknown of string

  let module_name = "core"

  let of_string = function
    | "next_item" -> Next_item
    | "prev_item" -> Prev_item
    | "leave_directory" -> Leave_directory
    | "enter_directory" -> Enter_directory
    | "change_active_pane" -> Change_active_pane
    | "copy" -> Copy
    | "move" -> Move
    | "delete" -> Delete
    | "rename" -> Rename
    | "jump" -> Jump
    | "quit" -> Quit
    | "toggle_mark" -> Toggle_mark
    | "make_dir" -> Make_dir
    | "history" -> History
    | "change_permission" -> Change_permission
    | "toggle_bookmark" -> Toggle_bookmark
    | _ as v-> Unknown v
end

type t =
    Core of Core.t
  | Thrid_party of string * string

let action_of_string module_name action =
  match module_name with
  | v when v = Core.module_name -> Core (Core.of_string action)
  | _ -> Thrid_party (module_name, action)

let of_string action =
  let has_splitter = String.index action ':' <> -1 in
  if has_splitter then begin
    let module_name = String.split_on_char ':' action in
    match module_name with
    | name :: [action] -> action_of_string name action
    | _ -> failwith (Printf.sprintf "Invalid format: %s" action)
  end
  else
    action_of_string Core.module_name action
