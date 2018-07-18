module T = Sxfiler_types
module Tj = Sxfiler_types_jsoo
module Rpc = Sxfiler_rpc

module Get_sync = struct
  include Rpc.Configuration.Get_sync

  let result_of_json = Tj.Configuration.of_js
end
