(** {!Rpc} module provides common signature between client and server to communicate same
    signature.
    Submodules in this module defines OCaml types and converter only, each JSON representations are
    defines in each package.
*)

module Completion = struct
  module Setup_file_sync = struct
    type param = {
      workspace_name: string
    }

    type result = unit
  end

  module Read_file_sync = struct
    type param = {
      input: string;
    }

    type result = Node.t Types.Candidate.t array
  end

  module Read_directory_sync = struct
    type param = {
      input: string;
    }

    type result = Directory_tree.t Types.Candidate.t array
  end

  module Read_history_sync = struct
    type param = {
      input: string;
    }

    type result = Snapshot_record.t Types.Candidate.t array
  end
end

module File = struct
  module Take_snapshot = struct
    type param = {
      directory: string;
      stack_name: string;
    }
  end
end

module Workspace = struct
  (** {!Make_sync} module defines interface to make workspace. *)
  module Make_sync = struct
    type param = {
      initial_directory: string;
      name: string;
    }

    type result = {
      created: bool;
    }
  end

  module Get_sync = struct
    type param = {
      name: string;
    }

    type result = Workspace.t
  end
end
