module T = Sxfiler_types

module Setup_sync = struct
  type params = {
    source: T.Completion.Common_item.t T.Completion.source;
  }

  type result = unit
  let name = "completion/setup/sync"
end

module Read_sync = struct
  type params = {
    input: string;
  }

  type result = T.Completion.Common_item.t T.Completion.result
  let name = "completion/read/sync"
end

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

  type result = T.Node.t T.Completion.result
  let name = "completion/read/file/sync"
end

module Read_history_sync = struct
  type params = {
    input: string;
  }

  type result = T.Location_record.t T.Completion.result
  let name = "completion/read/history/sync"
end
