(** defines types for task that are used widely in modules related task *)

type id = Uuidm.t [@@deriving show, eq]

module Context = struct
  type t = {task_id : id}
end
