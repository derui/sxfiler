(** {!Rpc} module provides common API definition between client and server to communicate same
    signature.
    Submodules in this module defines OCaml types and converter only, each JSON representations are
    defines in each package.

    Modules definied in this module can not use directly Jsonrpc, so will use specified implementations
    in jsoo/yojson package.
*)

module Rpc_completion = struct
  module Setup_file_sync = struct
    type params = {
      workspace_name: string
    }

    type result = unit
    let name = "completion/setup/file/sync"
  end

  module Read_file_sync = struct
    type params = {
      input: string;
    }

    type result = Node.t Types.Candidate.t array
    let name = "completion/read/file/sync"
  end

  module Read_directory_sync = struct
    type params = {
      input: string;
    }

    type result = Directory_tree.t Types.Candidate.t array
    let name = "completion/read/directory/sync"
  end

  module Read_history_sync = struct
    type params = {
      input: string;
    }

    type result = Snapshot_record.t Types.Candidate.t array
    let name = "completion/read/history/sync"
  end
end

module Rpc_file = struct
  module Take_snapshot = struct
    type params = {
      directory: string;
      stack_name: string;
    }

    type result = unit

    let name = "file/take_snapshot"
  end
end

module Rpc_workspace = struct
  (** {!Make_sync} module defines interface to make workspace. *)
  module Make_sync = struct
    type params = {
      initial_directory: string;
      name: string;
    }

    type result = {
      created: bool;
    }
    let name = "workspace/make/sync"
  end

  module Get_sync = struct
    type params = {
      name: string;
    }

    type result = Workspace.t
    let name = "workspace/get/sync"
  end
end

module Rpc_notification = struct
  module Workspace_update = struct
    type params = {
      name: string;
      workspace: Workspace.t;
    }

    type result = unit
    let name = "notification/workspace/update"
  end
end
