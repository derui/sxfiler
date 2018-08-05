include Sxfiler_domain.Condition

type js = {
  on_file_tree: bool [@key "onFileTree"][@default false];
  on_completing: bool [@key "onCompleting"][@default false];
}
[@@deriving yojson]

let to_yojson: t -> Yojson.Safe.json = fun t ->
  let module T = Sxfiler_domain in
  let list = T.Condition.to_list t in
  js_to_yojson {
    on_file_tree = List.mem T.Condition.On_file_tree list;
    on_completing = List.mem T.Condition.On_completing list;
  }

let of_yojson json =
  let open Ppx_deriving_yojson_runtime in
  js_of_yojson json >>= fun js ->
  let switch t ctx = function
    | true -> enable t ~context:ctx
    | false -> disable t ~context:ctx
  in
  let t = switch empty On_file_tree js.on_file_tree in
  let t = switch t On_completing js.on_completing in
  Ok t
