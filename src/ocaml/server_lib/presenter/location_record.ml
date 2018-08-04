include Sxfiler_domain.Location_record

module Js = struct
  type t = {
    location: string;
    timestamp: string;
  } [@@deriving yojson]
end

let to_yojson : t -> Yojson.Safe.json = fun t ->
  Js.to_yojson {
    Js.location = t.location;
    timestamp = Int64.to_string t.timestamp;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v -> Ok {
    location = v.Js.location;
    timestamp = Int64.of_string v.Js.timestamp;
  }
