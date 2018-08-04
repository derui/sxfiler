include Sxfiler_domain.Key_map

type js = {
  condition: Condition.t;
  key: string;
  value: Yojson.Safe.json[@key "action"];
}
[@@deriving yojson]

type js_list = js list [@@deriving yojson]

module type Conv = sig
  type t

  val to_yojson: t -> Yojson.Safe.json
  val of_yojson: Yojson.Safe.json -> (t, string) result
end

let to_yojson (type v) t ~conv:(module C:Conv with type t = v) =
  let bindings = bindings t in
  List.map (fun (cond, key, value) -> (cond, key, C.to_yojson value)) bindings
  |> List.map (fun (cond, key, value) -> {condition = cond;key = Sxfiler_kbd.to_keyseq key; value;})
  |> js_list_to_yojson

let of_yojson (type v) js ~conv:(module C:Conv with type t = v) =
  let open Ppx_deriving_yojson_runtime in
  js_list_of_yojson js >>= fun js_list ->
  Ok (List.fold_left (fun keymap js ->
      match (C.of_yojson js.value, Sxfiler_kbd.of_keyseq js.key) with
      | Ok value, Some key -> add keymap ~condition:js.condition ~key ~value
      | _, _ -> keymap
    )
      empty
      js_list)
