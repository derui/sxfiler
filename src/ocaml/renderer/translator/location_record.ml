open Sxfiler_core
include Sxfiler_domain.Location_record

class type js = object
  method location: Js.js_string Js.t Js.readonly_prop
  method timestamp: Js.js_string Js.t Js.readonly_prop
end

let of_js js : t =
  {
    location = Path.of_string @@ Js.to_string js##.location;
    timestamp = Int64.of_string @@ Js.to_string js##.timestamp;
  }

let to_js t : js Js.t = object%js
  val location = Js.string @@ Path.to_string t.location
  val timestamp = Js.string @@ Int64.to_string t.timestamp
end
