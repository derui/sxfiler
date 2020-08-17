open Sxfiler_core
module D = Sxfiler_domain.Theme
module Gen = Sxfiler_generated

type error =
  | Empty_name
  | Empty_color_key
  | Illegal_theme
  | Invalid_color_code of string
[@@deriving eq, show]

(* translator between domain and response/request type *)
let of_domain (t : D.color_pairs) =
  let module E = Sxfiler_domain.Common.Not_empty_string in
  let color_pairs =
    List.map
      (fun (key, color_code) ->
        { Gen.Theme.ColorPair.name = E.value key; hex_color = D.Color_code.to_string color_code })
      t
  in
  { Gen.Theme.ColorTheme.color_pairs }

let to_domain (t : Gen.Theme.ColorTheme.t) =
  let module E = Sxfiler_domain.Common.Not_empty_string in
  let open Result.Infix in
  let* color_pairs =
    List.fold_left
      (fun color_codes (color_pair : Gen.Theme.ColorPair.t) ->
        let* key = E.make color_pair.name |> Option.to_result ~none:Empty_color_key in
        let* color =
          D.Color_code.of_string color_pair.hex_color
          |> Option.to_result ~none:(Invalid_color_code color_pair.hex_color)
        in
        Result.fmap color_codes ~f:(fun v -> (key, color) :: v))
      (Ok []) t.color_pairs
  in
  Ok color_pairs
