(* support functions to create class name for component *)
let make list = Js.Optdef.return @@ Js.string @@ String.concat " " list

let join (cls, condition) list = if condition then cls :: list else list

module Infix = struct
  let (<|>) = join
end

module Style = struct
  let grid_container = join ("grid", true)
  let grid_item = join ("grid__item", true)
end
