(** Locate application information. This module as is singleton. *)
open Sxfiler_renderer_core
module S = Sxfiler_renderer_store

include Locator_intf

module type Main = S with type store = Sxfiler_renderer_store.App.Store.t
                      and type message = Sxfiler_renderer_core.Message.t

module Make
    (R:Rpc_intf.Rpc)
    (Context: Context_intf.Instance with type store := S.App.Store.t
                                     and type message := Message.t): Main = struct
  type store = S.App.Store.t
  type message = Message.t

  module Rpc = R
  module Context = Context

  module Repository = struct
    module Scanner = (struct
      module Repo = Repository.Scanner
      let instance = Repo.make ()
    end : Repository_intf.Scanner_instance)

    module Keybindings = struct
      module Repo = Repository.Keybindings
      let instance = Repo.make ()
    end

    module Configuration = struct
      module Repo = Repository.Configuration
      let instance = Repo.make ()
    end
  end
end
