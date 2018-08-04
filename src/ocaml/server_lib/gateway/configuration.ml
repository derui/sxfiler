module T = Sxfiler_domain
module P = Sxfiler_server_presenter
module Rpc = Sxfiler_rpc

module Get_sync = struct
  include Rpc.Configuration.Get_sync

  let result_to_yojson = P.Configuration.to_yojson
  let result_of_yojson = P.Configuration.of_yojson
end
