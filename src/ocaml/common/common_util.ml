(* Utilities for binding js_of_ocaml *)

module Option = struct
  exception None_of_object

  let get ~default = function
    | Some v -> v
    | None -> default

  let get_exn = function
    | Some v -> v
    | None -> raise None_of_object
end
