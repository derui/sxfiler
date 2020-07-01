open Abbrev
open Sxfiler_core
module S = Common_step_theme

type error =
  | Invalid_color_format of (string * string) list
  | Store_error          of S.Store_theme.error
  | Remove_error         of S.Remove_theme.error
[@@deriving eq, show]

module Add_theme = struct
  type input = {
    name : D.Common.Not_empty_string.t;
    description : D.Common.Not_empty_string.t option;
    color_codes : (D.Common.Not_empty_string.t * D.Common.Not_empty_string.t) list;
  }

  type work_flow = input -> (D.Theme.t, error) result Lwt.t
end

module Remove_theme = struct
  type input = { name : D.Common.Not_empty_string.t }

  type work_flow = input -> (unit, error) result Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

module List_theme = struct
  type input = unit

  type work_flow = input -> D.Theme.t list Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

type commands =
  | Add_theme    of Add_theme.input
  | Remove_theme of Remove_theme.input
  | List_theme   of List_theme.input

module S_set = Set.Make (struct
  type t = D.Common.Not_empty_string.t

  let compare = D.Common.Not_empty_string.compare
end)

let add_theme : Common_step_theme.Store_theme.t -> Add_theme.work_flow =
 fun store_theme { name; description; color_codes } ->
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
    let theme = D.Theme.make ~name ?description ~colors:color_codes' () in
    match%lwt store_theme theme with
    | Ok ()       -> Lwt.return_ok theme
    | Error error -> Lwt.return_error (Store_error error)

let remove_theme : S.List_theme.t -> S.Remove_theme.t -> Remove_theme.work_flow =
 fun list_theme remove_theme { name } ->
  let module E = D.Common.Not_empty_string in
  let%lwt themes = list_theme () in
  match List.find_opt (fun theme -> E.equal theme.D.Theme.name name) themes with
  | None       -> Lwt.return_error (Remove_error (S.Remove_theme.Not_found_theme (E.value name)))
  | Some theme -> remove_theme theme |> Lwt_result.map_err (fun e -> Remove_error e)

let list_theme : S.List_theme.t -> List_theme.work_flow = fun list_theme () -> list_theme ()
