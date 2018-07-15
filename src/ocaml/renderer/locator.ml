(** Locate application information. This module as is singleton. *)
open Sxfiler_renderer_core

include Locator_intf

module Make(R:Rpc_intf.Rpc) : S = struct
  module Rpc = R

  module Repository = struct
    module Scanner = (struct
      module Repo = Repository.Scanner
      let instance = Repo.make ()
    end : Repository_intf.Scanner_instance)
  end
end
