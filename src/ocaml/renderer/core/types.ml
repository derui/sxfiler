module Ty = Sxfiler_types

module File_tree = struct
  type tree = {
    scanner: Ty.Scanner.t;
    selected_item_index: int;
  }

  type t = {
    scanners: tree Jstable.t;
    scanner_order: string * string;
  }

  let make order =
    {
      scanners = Jstable.create ();
      scanner_order = order;
    }

  let swap_order t = {t with scanner_order = Sxfiler_core.Tuple.swap t.scanner_order}

  let update t ~scanner =
    Jstable.add t.scanners Js.(string scanner.Ty.Scanner.name) {scanner; selected_item_index = 0};
    t

  let to_list t =
    let order1, order2 = t.scanner_order in
    let list = [
      Jstable.find t.scanners Js.(string order1) |> Js.Optdef.to_option;
      Jstable.find t.scanners Js.(string order2) |> Js.Optdef.to_option;
    ] in
    let open Sxfiler_core.Option in
    List.filter is_some list |> List.map get_exn

end
