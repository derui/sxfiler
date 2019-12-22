open Sxfiler_server_generated

include
  Core.Domain_translator
    with type t := Filer.LocationHistory.t
     and type domain := Sxfiler_domain.Location_history.t
