module T = Sxfiler_domain

module Setup_sync = struct
  type params = {
    source: T.Completion.collection;
  }

  type result = unit
  let name = "completion/setup/sync"
end

module Read_sync = struct
  type params = {
    input: string;
  }

  type result = T.Completion.result
  let name = "completion/read/sync"
end
