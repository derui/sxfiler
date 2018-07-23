open Sxfiler_types.Completion

module Candidate = struct
  type js = {
    start: int;
    length: int;
    value: Yojson.Safe.json;
  } [@@deriving yojson]
end

module Common_item = struct
  include Common_item

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
