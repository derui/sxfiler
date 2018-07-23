include Command_intf

module Jump : S = struct
  type param = [
    | `Directory of string
    | `Scanner of string
  ]

  let def_to_param def value =
    match def.C.Param_def.name with
    | "directory" -> Some (`Directory (Yojson.Safe.Util.to_string value))
    | "scanner" -> Some (`Scanner (Yojson.Safe.Util.to_string value))
    | _ -> None

  type t = {
    params: param Param_map.t;
  }

  let make () = {params = Param_map.empty}

  let definition = {
    C.command_class = C.Class.Jump;
    param_defs = [
      {C.Param_def.name = "directory"; typ = C.Param_type.Single_node;};
      {C.Param_def.name = "target"; typ = C.Param_type.String;};
    ]
  }

  let set_param t ~def ~value =
    match def_to_param def value with
    | None -> t
    | Some param -> {params = Param_map.add def.C.Param_def.name param t.params}

  let plan = `No_side_effect

  let execute _ = Lwt.return_unit

end
