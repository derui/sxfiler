module T = Sxfiler_types

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

  type result = T.Node.t T.Types.Candidate.t array
  let name = "completion/read/file/sync"
end

module Read_directory_sync = struct
  type params = {
    input: string;
  }

  type result = T.Directory_tree.t T.Types.Candidate.t array
  let name = "completion/read/directory/sync"
end

module Read_history_sync = struct
  type params = {
    input: string;
  }

  type result = T.Snapshot_record.t T.Types.Candidate.t array
  let name = "completion/read/history/sync"
end
