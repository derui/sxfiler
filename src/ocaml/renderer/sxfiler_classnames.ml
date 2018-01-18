(* support functions to create class name for component *)
let make list = Js.Optdef.return @@ Js.string @@ String.concat " " @@ List.rev list

let join (cls, condition) list = if condition then cls :: list else list

module Infix = struct
  let (<|>) cls list = join list cls
end
