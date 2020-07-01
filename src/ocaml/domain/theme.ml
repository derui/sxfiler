open Sxfiler_core

(* base color type *)
module Color = struct
  type t = int [@@deriving eq]

  let of_int v = v

  let to_int v = v

  let int_to_hex = function
    | _ as v when 0 <= v && v <= 9 -> Char.chr (v + Char.code '0') |> Char.escaped
    | _ as v when 10 <= v && v <= 15 -> Char.chr (v - 10 + Char.code 'A') |> Char.escaped
    | _ -> failwith "Invalid integer"

  let show : t -> string =
   fun v ->
    let under = v land 0xf and upper = (v lsr 4) land 0xf in
    int_to_hex upper ^ int_to_hex under

  let pp fmt t = Format.fprintf fmt "%s" (show t)
end

(* colors *)
type red = Color.t [@@deriving show, eq]

type green = Color.t [@@deriving show, eq]

type blue = Color.t [@@deriving show, eq]

type alpha = Color.t [@@deriving show, eq]

module Color_code = struct
  type t =
    | RGB  of (red * green * blue)
    | RGBA of (red * green * blue * alpha)
  [@@deriving show, eq]

  let hex_color_base = Re.Posix.compile_pat "^#([0-9a-fA-F]+)$"

  let hex_to_int str =
    let color = ref 0 in
    String.iter
      (fun c ->
        let v =
          match Char.uppercase_ascii c with
          | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> Char.code c - Char.code '0'
          | ('A' | 'B' | 'C' | 'D' | 'E' | 'F') as c -> Char.code c - Char.code 'A' + 10
          | _ -> 0
        in
        color := (!color lsl 4) lor v)
      str;
    !color

  let of_string str =
    if String.length str = 0 then None
    else
      let open Option.Infix in
      (Re.exec_opt hex_color_base str >>| fun group -> Re.Group.get group 1) >>= fun hex ->
      let hex_to_color v = hex_to_int v |> Color.of_int in
      match String.length hex with
      | 3 | 4 -> (
          let colors = String.split_by_len ~len:1 hex in
          match colors with
          | [ r; g; b ]    -> Some (RGB (hex_to_color (r ^ r), hex_to_color (g ^ g), hex_to_color (b ^ b)))
          | [ r; g; b; a ] ->
              Some (RGBA (hex_to_color (r ^ r), hex_to_color (g ^ g), hex_to_color (b ^ b), hex_to_color (a ^ a)))
          | _              -> failwith "Invalid state" )
      | 6 | 8 -> (
          let colors = String.split_by_len ~len:2 hex in
          match colors with
          | [ r; g; b ]    -> Some (RGB (hex_to_color r, hex_to_color g, hex_to_color b))
          | [ r; g; b; a ] -> Some (RGBA (hex_to_color r, hex_to_color g, hex_to_color b, hex_to_color a))
          | _              -> failwith "Invalid state" )
      | _     -> None

  let to_string = function
    | RGB (r, g, b)     -> "#" ^ Color.show r ^ Color.show g ^ Color.show b
    | RGBA (r, g, b, a) -> "#" ^ Color.show r ^ Color.show g ^ Color.show b ^ Color.show a
end

type str = Common.Not_empty_string.t

(* map of color codes *)
module Color_map = struct
  include Map.Make (struct
    type t = str

    let compare = Common.Not_empty_string.compare
  end)

  let pp pp_v fmt t =
    let list = Fmt.list & Fmt.pair Common.Not_empty_string.pp pp_v in
    let formatter = Fmt.box list in
    let entries = bindings t in
    formatter fmt entries
end

type t = {
  name : Common.Not_empty_string.t;
  description : Common.Not_empty_string.t option;
  colors : Color_code.t Color_map.t;
}
[@@deriving show, eq]

let make ~name ?description ~colors () =
  let colors = List.fold_left (fun accum (key, color) -> Color_map.add key color accum) Color_map.empty colors in
  { name; description; colors }
