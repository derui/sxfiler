include Command_intf

module Registry = struct

  type t = (module Instance) Jstable.t

  let make () = Jstable.create ()
  let register t instance =
    let module I = (val instance : Instance) in
    Jstable.add t I.(Js.string @@ Command.name this) instance;
    t

  let get t ~action =
    let name = Callable_action.to_string action in
    Js.Optdef.to_option @@ Jstable.find t Js.(string name)

  let to_action_list t =
    Jstable.keys t
    |> List.map Js.to_string
    |> List.map (fun v -> Callable_action.of_string v)
end
