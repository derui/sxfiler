(** Persist current status of the application *)

module S = Sxfiler_domain
module T = Sxfiler_server_translator

type filer_stat =
  { id : string
  ; name : string
  ; location : string
  ; history : T.Location_history.t }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

type t = {filers : filer_stat list}
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let empty = {filers = []}

(** [add_filer_stat filer stat] add the filer to stat *)
let add_filer_stat (filer : S.Filer.t) stat =
  let filer' = T.Filer.of_domain filer in
  let value =
    { id = filer'.id
    ; name = filer'.name
    ; location = filer'.file_list.location
    ; history = filer'.history }
  in
  {filers = value :: stat.filers}
