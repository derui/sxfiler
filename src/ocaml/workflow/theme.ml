open Abbrev
open Sxfiler_core
module T = Common_step_theme
include Theme_intf

module S_set = Set.Make (struct
  type t = D.Common.Not_empty_string.t

  let compare = D.Common.Not_empty_string.compare
end)

let update_theme { Update_theme.base_theme; color_codes } =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_theme_instance c) in
  let module Instance = (val instance : Common_step_theme.Instance) in
  let module E = D.Common.Not_empty_string in
  let color_keys = List.map fst color_codes |> S_set.of_list in
  let color_codes' =
    List.fold_left
      (fun acc (key, code) ->
        match E.value code |> D.Theme.Color_code.of_string with Some code -> (key, code) :: acc | None -> acc)
      [] color_codes
  in
  let converted_keys = List.map fst color_codes' |> S_set.of_list in
  let diff = S_set.diff color_keys converted_keys in
  if not & S_set.is_empty diff then
    let errors =
      S_set.fold
        (fun v acc ->
          match List.assoc_opt v color_codes with Some code -> (E.value v, E.value code) :: acc | None -> acc)
        diff []
    in
    S.return_error (Invalid_color_format errors)
  else
    let* pairs = Instance.store_theme color_codes' base_theme |> S.return_lwt in
    match pairs with Ok pairs -> S.return_ok pairs | Error error -> S.return_error (Store_error error)

let get_theme () =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_theme_instance c) in
  let module Instance = (val instance : Common_step_theme.Instance) in
  Instance.get_current_theme () |> S.return_lwt
