include Core_intf

module Make_registry (Com : Registry.Command) : Registry.S with type command := Com.t = struct
  type t = Com.t Jstable.t

  let make () = Jstable.create ()

  let register t command =
    let name = Com.to_name command in
    Jstable.add t (Js.string name) command ;
    t

  let get t ~name = Js.Optdef.to_option @@ Jstable.find t Js.(string name)
  let names t = Jstable.keys t |> List.map Js.to_string
end

module Static_registry = Make_registry (struct
    type t = static_command

    let to_name t = t.name
  end)

module Dynamic_registry = Make_registry (struct
    type t = (module Instance)

    let to_name (module C : Instance) = C.(Command.name this)
  end)
