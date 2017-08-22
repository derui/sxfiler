(* support functions to create class name for component *)
let make list = Js.Optdef.return @@ Js.string @@ String.concat " " list

module Style = struct
  let grid_container list = "grid" :: list
  let grid_item list = "grid__item" :: list
end
