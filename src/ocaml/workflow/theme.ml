open Abbrev
open Sxfiler_core
module S = Common_step_theme

type error =
  | Invalid_color_format of (string * string) list
  | Store_error          of S.Store_theme.error
[@@deriving eq, show]

module Update_theme = struct
  type input = {
    base_theme : D.Common.Not_empty_string.t option;
    color_codes : (D.Common.Not_empty_string.t * D.Common.Not_empty_string.t) list;
  }

  type work_flow = input -> (D.Theme.color_pairs, error) result Lwt.t
end

module Get_theme = struct
  type input = unit

  type work_flow = input -> D.Theme.color_pairs Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

type commands =
  | Update_theme of Update_theme.input
  | Get_theme    of Get_theme.input

module S_set = Set.Make (struct
  type t = D.Common.Not_empty_string.t

  let compare = D.Common.Not_empty_string.compare
end)

let update_theme : Common_step_theme.Store_theme.t -> Update_theme.work_flow =
 fun store_theme { base_theme; color_codes } ->
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
    Lwt.return_error (Invalid_color_format errors)
  else
    match%lwt store_theme color_codes' base_theme with
    | Ok pairs    -> Lwt.return_ok pairs
    | Error error -> Lwt.return_error (Store_error error)

let get_theme : S.Get_current_theme.t -> Get_theme.work_flow = fun get_current_theme () -> get_current_theme ()
