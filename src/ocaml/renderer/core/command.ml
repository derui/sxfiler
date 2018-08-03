include Command_intf

module Make_registry(Com:Registry.Command) : Registry.S with type command := Com.t = struct

  type t = Com.t Jstable.t

  let make () = Jstable.create ()
  let register t command =
    let name = Com.to_name command in
    Jstable.add t (Js.string name) command;
    t

  let get t ~action =
    let name = Callable_action.to_string action in
    Js.Optdef.to_option @@ Jstable.find t Js.(string name)

  let to_action_list t =
    Jstable.keys t
    |> List.map Js.to_string
    |> List.map (fun v -> Callable_action.of_string v)
end
