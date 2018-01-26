(* Utilities for binding js_of_ocaml *)

module Option = struct
  let map ~f v = match v with
    | None -> None
    | Some v -> Some (f v)

  let bind v f = match v with
    | None -> None
    | Some v -> f v

  let return v = Some v

  module Infix = struct
    let (>>=) = bind
    let return = return
  end
end
