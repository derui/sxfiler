(* Utilities for binding js_of_ocaml *)

module Option = struct
  exception None_of_object

  let is_some = function
    | Some _ -> true
    | None -> false

  let is_none v = not @@ is_some v

  let get ~default = function
    | Some v -> v
    | None -> default

  let get_exn = function
    | Some v -> v
    | None -> raise None_of_object
end
