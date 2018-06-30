(** {!Rpc} module provides common signature between client and server to communicate same
    signature.
    Submodules in this module defines OCaml types and converter only, each JSON representations are
    defines in each package.
*)

module Completion = struct
  module Setup = struct
    type param = {
      source_type : Types.Source_type.t;
    }

    type result = unit
  end

  module Read_file = struct
    type param = {
      input: string;
    }

    type result = Node.t Types.Candidate.t array
  end

  module Read_directory = struct
    type param = {
      input: string;
    }

    type result = Directory_tree.t Types.Candidate.t array
  end

  module Read_history = struct
    type param = {
      input: string;
    }

    type result = Snapshot_record.t Types.Candidate.t array
  end
end
