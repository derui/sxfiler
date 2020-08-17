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

type color_pairs = (Common.Not_empty_string.t * Color_code.t) list

module Definition = struct
  type t = {
    name : Common.Not_empty_string.t;
    description : Common.Not_empty_string.t option;
    colors : Color_code.t Color_map.t;
    base : t option;
  }
  [@@deriving show, eq]

  let base_colors =
    [
      ("base03", Color_code.of_string "#002b36" |> Option.get);
      ("base02", Color_code.of_string "#073642" |> Option.get);
      ("base01", Color_code.of_string "#586e75" |> Option.get);
      ("base00", Color_code.of_string "#657b83" |> Option.get);
      ("base0", Color_code.of_string "#839496" |> Option.get);
      ("base1", Color_code.of_string "#93a1a1" |> Option.get);
      ("base2", Color_code.of_string "#eee8d5" |> Option.get);
      ("base3", Color_code.of_string "#fdf6e3" |> Option.get);
      ("yellow", Color_code.of_string "#b58900" |> Option.get);
      ("orange", Color_code.of_string "#cb4b16" |> Option.get);
      ("red", Color_code.of_string "#dc322f" |> Option.get);
      ("magenta", Color_code.of_string "#d33682" |> Option.get);
      ("violet", Color_code.of_string "#6d71c4" |> Option.get);
      ("blue", Color_code.of_string "#268bd2" |> Option.get);
      ("cyan", Color_code.of_string "#2aa198" |> Option.get);
      ("green", Color_code.of_string "#859900" |> Option.get);
    ]
    |> List.fold_left
         (fun map (name, color) -> Color_map.add (Common.Not_empty_string.make name |> Option.get) color map)
         Color_map.empty

  let base =
    let name = Common.Not_empty_string.make "default" |> Option.get
    and description = Common.Not_empty_string.make "The theme for default" in
    let color name = Color_map.find (Common.Not_empty_string.make name |> Option.get) base_colors in
    let colors =
      [
        ("ui.switchRail", color "base03");
        ("ui.switchRailChecked", color "blue");
        ("ui.switchBox", color "base3");
        ("completer.candidateText", color "base3");
        ( "completer.selectedCandidateBackground",
          Printf.sprintf "%s4d" (color "base2" |> Color_code.to_string) |> Color_code.of_string |> Option.get );
        ("completer.matchingAreaText", color "base03");
        ("completer.matchingAreaBackground", color "yellow");
        ("completer.baseBackground", color "base02");
        ("completer.titleText", color "base03");
        ("completer.titleBackground", color "base3");
        ("completer.inputBorder", color "blue");
        ("completer.inputText", color "base3");
        ("completer.inputBackground", color "base03");
        ("configuration.cellSeparator", color "base3");
        ("configuration.cellBackground", color "base02");
        ("configuration.cellText", color "base3");
        ("configuration.shadowCellFocused", color "orange");
        ("configuration.selectBackground", color "base3");
        ("configuration.dropdownMark", color "base03");
        ("configuration.selectionMenuText", color "base03");
        ("configuration.selectionMenuBackground", color "base3");
        ("configuration.selectionOptionText", color "base3");
        ("configuration.selectionOptionBackground", color "cyan");
        ("configuration.selectionOptionSeparator", color "base01");
        ("configuration.navigatorBackground", color "base02");
        ("configuration.navigatorText", color "base3");
        ("configuration.navigatorCatgegoryMarker", color "base3");
        ("configuration.navigatorSectionText", color "base3");
        ("configuration.navigatorSectionHover", color "orange");
        ("configuration.sectionBackground", color "base03");
        ("configuration.sectionHeaderText", color "base3");
        ("configuration.sectionHeaderBackground", color "base02");
        ("configuration.sectionCellBackground", color "base03");
        ("decisionModal.headerText", color "base03");
        ("decisionModal.headerBackground", color "base3");
        ("decisionModal.panelText", color "base3");
        ("decisionModal.panelBackground", color "base03");
        ("decisionModal.panelSelectedText", color "base03");
        ("decisionModal.panelSelectedBackground", color "base3");
        ("decisionModal.panelSelectedMarker", color "red");
        ("fileItem.baseText", color "base2");
        ("fileItem.baseBackground", color "base03");
        ("fileItem.selectedMarker", color "orange");
        ( "fileItem.mark",
          Printf.sprintf "%s3f" (color "blue" |> Color_code.to_string) |> Color_code.of_string |> Option.get );
        ("fileItem.bookmark", color "cyan");
        ("fileItem.directoryText", color "yellow");
        ("fileItem.symlinkText", color "orange");
        ("fileList.baseText", color "base01");
        ("fileList.baseBackground", color "base03");
        ("fileList.emptyText", color "red");
        ("fileList.separator", color "base0");
        ("fileList.headerText", color "base1");
        ("fileList.headerBackground", color "base03");
        ("fileList.headerBorder", color "base01");
        ("fileList.focusedText", color "yellow");
        ("logViewer.text", color "base2");
        ("logViewer.background", color "base03");
        ("logViewer.infoLevel", color "blue");
        ("logViewer.warningLevel", color "orange");
        ("logViewer.errorLevel", color "red");
        ("scrollBar.background", color "base01");
        ("configurationEditor.background", color "base3");
        ("fileList.separator", color "base00");
        ("logViewer.separator", color "base00");
      ]
      |> List.fold_left
           (fun map (name, color) -> Color_map.add (Common.Not_empty_string.make name |> Option.get) color map)
           Color_map.empty
    in
    { description; name; colors; base = None }

  let make ?description ?base:base' ~name ~colors () =
    let colors = List.fold_left (fun accum (key, color) -> Color_map.add key color accum) Color_map.empty colors in
    let base' = match base' with None -> Some base | _ as v -> v in
    { name; description; colors; base = base' }

  let extract_color { colors; base; _ } =
    Option.map (fun { colors; _ } -> colors) base
    |> Option.value ~default:Color_map.empty
    |> Color_map.merge (fun _ base original -> match original with Some _ -> original | None -> base) colors
end

module Configuration = struct
  type t = {
    colors : Color_code.t Color_map.t;
    base : Definition.t;
  }

  let make ~colors ?base () =
    {
      colors = List.fold_left (fun acc (key, value) -> Color_map.add key value acc) Color_map.empty colors;
      base = Option.value ~default:Definition.base base;
    }

  let merge_color t =
    let colors = Definition.extract_color t.base in
    Color_map.merge (fun _ base override -> match override with None -> base | Some _ -> override) colors t.colors
    |> Color_map.to_seq |> List.of_seq
end
