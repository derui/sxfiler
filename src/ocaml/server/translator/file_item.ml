open Sxfiler_core
module D = Sxfiler_domain.File_item

type t =
  { id : string
  ; parent : string
  ; name : string
  ; full_path : string [@key "fullPath"]
  ; stat : File_stat.t
  ; link_path : string option [@key "linkPath"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

module Domain = struct
  let of_domain t =
    { id = t.D.id
    ; name = Path.basename t.D.full_path
    ; full_path = Path.to_string t.D.full_path
    ; stat = File_stat.of_domain t.D.stat
    ; parent = Path.dirname t.D.full_path
    ; link_path = Option.(t.link_path >|= fun v -> Path.to_string v) }

  let to_domain t =
    { D.id = t.id
    ; full_path = Path.of_string t.full_path
    ; stat = File_stat.to_domain t.stat
    ; link_path = Option.(t.link_path >|= fun v -> Path.of_string v) }
end

include Domain
