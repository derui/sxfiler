open Sxfiler_core
module D = Sxfiler_domain.Theme
module Gen = Sxfiler_generated

type error =
  | Empty_name
  | Empty_color_key
  | Invalid_color_code of string
[@@deriving eq, show]

(* translator between domain and response/request type *)
let of_domain (t : D.t) =
  let module E = Sxfiler_domain.Common.Not_empty_string in
  let color_codes = D.Color_map.bindings t.colors in
  let color_codes =
    List.map
      (fun (key, color_code) ->
        { Gen.Theme.ColorCode.name = E.value key; hex_color = D.Color_code.to_string color_code })
      color_codes
  in
  {
    Gen.Theme.Theme.name = E.value t.name;
    description = Option.map E.value t.description |> Option.value ~default:"";
    color_codes;
  }

let to_domain (t : Gen.Theme.Theme.t) =
  let module E = Sxfiler_domain.Common.Not_empty_string in
  let open Result.Infix in
  let* name = E.make t.name |> Option.to_result ~none:Empty_name in
  let description = E.make t.description in
  let* color_codes =
    List.fold_left
      (fun color_codes (color_code : Gen.Theme.ColorCode.t) ->
        let* key = E.make color_code.name |> Option.to_result ~none:Empty_color_key in
        let* color =
          D.Color_code.of_string color_code.hex_color
          |> Option.to_result ~none:(Invalid_color_code color_code.hex_color)
        in
        Result.fmap color_codes ~f:(fun v -> (key, color) :: v))
      (Ok []) t.color_codes
  in
  Ok (D.make ~name ?description ~colors:color_codes ())
