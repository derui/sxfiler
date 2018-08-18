open Sxfiler_rpc.Types.Location_record

class type js = object
  method location: Js.js_string Js.t Js.readonly_prop
  method timestamp: Js.js_string Js.t Js.readonly_prop
end

let of_js js : t =
  {
    location = Js.to_string js##.location;
    timestamp = Js.to_string js##.timestamp;
  }

let to_js t : js Js.t = object%js
  val location = Js.string t.location
  val timestamp = Js.string t.timestamp
end
