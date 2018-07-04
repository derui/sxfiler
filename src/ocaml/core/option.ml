
(** [is_none v] return [v] is {!None} or not *)
let is_none = function
  | None -> true
  | Some _ -> false

(** [is_some v] return [v] is {!Some} or not *)
let is_some v = not @@ is_none v

exception Not_some

(** [get_exn v] return wrapped value of [v]. Raise {!Not_some} if [v] is None.  *)
let get_exn = function
  | None -> raise Not_some
  | Some v -> v

(** [get ~default v] return wrapped value of [v]. If [v] is None, this returns [default]. *)
let get ~default = function
  | None -> default
  | Some v -> v
