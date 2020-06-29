open Sxfiler_core

type _ typ =
  | Int : int typ
  | String : string typ
  | Bool : bool typ
  | Pair   : ('a typ * 'b typ) -> ('a * 'b) typ
  | List   : 'a typ -> 'a list typ

let ( %* ) a b = Pair (a, b)

type state = State : ('a typ * 'a) -> state

type (_, _) eq = Eq : ('a, 'a) eq

let rec eq_type : type a b. a typ -> b typ -> (a, b) eq option =
 fun a b ->
  match (a, b) with
  | Int, Int                     -> Some Eq
  | String, String               -> Some Eq
  | Bool, Bool                   -> Some Eq
  | Pair (a1, a2), Pair (b1, b2) -> (
      match (eq_type a1 b1, eq_type a2 b2) with Some Eq, Some Eq -> Some Eq | _, _ -> None )
  | List a, List b               -> ( match eq_type a b with Some Eq -> Some Eq | _ -> None )
  | _, _                         -> None

let try_cast : type a. a typ -> state -> a option =
 fun typ (State (ty, v)) -> match eq_type typ ty with Some Eq -> Some v | None -> None

type app_state = (string, state) Hashtbl.t

let create () = Hashtbl.create 0

let read_state : type a. typ:a typ -> key:string -> app_state -> a option =
 fun ~typ ~key app_state ->
  let open Option.Infix in
  let* state = Hashtbl.find_opt app_state key in
  try_cast typ state

let write_state : type a. typ:a typ -> key:string -> value:a -> app_state -> unit =
 fun ~typ ~key ~value app_state -> Hashtbl.add app_state key (State (typ, value))

let to_json : app_state -> Yojson.Basic.t =
 fun app_state ->
  let assocs = Hashtbl.fold (fun key value lst -> (key, `String (Marshal.to_string value [])) :: lst) app_state [] in
  `Assoc assocs

let of_json json =
  match Yojson.Basic.Util.(filter_assoc [ json ]) with
  | []          -> create ()
  | assocs :: _ ->
      let app_state = create () in
      assocs
      |> List.iter (fun (key, value) ->
             match value with
             | `String value ->
                 let value = Marshal.from_string value 0 in
                 Hashtbl.add app_state key value
             | _             -> ());
      app_state
