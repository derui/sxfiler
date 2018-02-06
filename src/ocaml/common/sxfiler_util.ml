(* Utilities for binding js_of_ocaml *)

module Option = struct
  let map ~f v = match v with
    | None -> None
    | Some v -> Some (f v)

  let bind v f = match v with
    | None -> None
    | Some v -> f v

  let return v = Some v

  let get ~default = function
    | Some v -> v
    | None -> default

  module Infix = struct
    let (>>=) = bind
    let (>|=) v f = map ~f v
    let return = return
  end
end
