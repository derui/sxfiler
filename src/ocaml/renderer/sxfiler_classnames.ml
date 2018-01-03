(* support functions to create class name for component *)
let make list = Js.Optdef.return @@ Js.string @@ String.concat " " list

let join (cls, condition) list = if condition then cls :: list else list

module Infix = struct
  let (<|>) cls list = join list cls
end

module Style = struct
  module Grid = struct
    let container = ("grid", true)
    let item = ("grid__item", true)
    let item_row row = (Printf.sprintf "grid__item--row-%d" row, true)
    let item_col col = (Printf.sprintf "grid__item--col-%d" col, true)
    let item_span_row row span = (Printf.sprintf "grid__item--row-%d-span-%d" row span, true)
    let item_span_col col span = (Printf.sprintf "grid__item--col-%d-span-%d" col span, true)
  end
end
