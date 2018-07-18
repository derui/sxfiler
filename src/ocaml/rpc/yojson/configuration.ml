module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc

module Get_sync = struct
  include Rpc.Configuration.Get_sync

  let result_to_yojson = Ty.Configuration.to_yojson
end
