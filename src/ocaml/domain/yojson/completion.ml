open Sxfiler_domain.Completion

module Source_class = struct
  include Source_class

  let of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or = fun js ->
    match js with
    | `Int v -> begin match of_enum v with
        | None -> Error "Unknown source type"
        | Some v -> Ok v
      end
    | _ -> Error "Unknown constructor"

  let to_yojson : t -> Yojson.Safe.json = fun t -> `Int (to_enum t)
end

module Item = struct
  include Item

  module Js = struct
    type t = {
      id: string;
      value: string
    } [@@deriving yojson]
  end

  let to_yojson t = Js.to_yojson {Js.id = t.id;value = t.value;}
  let of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.of_yojson js >>= fun js -> Ok {id = js.Js.id;value = js.Js.value;}
end

module Candidate = struct
  include Candidate

  module Js = struct
    type t = {
      start: int;
      length: int;
      value: Item.t;
    } [@@deriving yojson]
  end

  let to_yojson t = Js.to_yojson {Js.start = t.start;
                                  length = t.length;
                                  value = t.value;}
  let of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.of_yojson js >>= fun js -> Ok {start = js.Js.start;
                                      length = js.Js.length;
                                      value = js.Js.value;}
end
