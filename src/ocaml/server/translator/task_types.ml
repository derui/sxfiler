module Ty = Sxfiler_domain.Task_types
module T = Sxfiler_domain.Task
module D = Sxfiler_domain.Task_interaction

module Task_id = struct
  type t = string [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  let of_domain id = Ty.show_id id

  let to_domain id =
    let open Sxfiler_core in
    Uuidm.of_string id |> Option.get_exn
end
