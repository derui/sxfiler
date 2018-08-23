open Sxfiler_rpc.Types.Condition

type js = Js.js_string Js.t Js.js_array

let to_js t : js Js.t = Js.array @@ Array.of_list @@ List.map Js.string t

let of_js : js Js.t -> t = fun js -> js |> Js.to_array |> Array.to_list |> List.map Js.to_string;
