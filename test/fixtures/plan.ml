module D = Sxfiler_domain

let state_for_fixture = Random.State.make_self_init ()

module Dummy = struct
  let do_plan _ = Lwt.return_ok ()
end

(** make fixture for {!D.Location_record} *)
let fixture ?(executor = (module Dummy : D.Plan.Executor)) target_nodes =
  let id = Uuidm.(v4_gen state_for_fixture () |> to_string) in
  D.Plan.make ~id ~executor ~target_nodes

let in_memory_repository () = failwith "not implementation"

(** make dummy factory  *)
let dummy_factory id =
  ( module D.Plan.Factory.Make (struct
        type id = D.Plan.id

        let generate () = id
      end)
      : D.Plan.Factory.S )
