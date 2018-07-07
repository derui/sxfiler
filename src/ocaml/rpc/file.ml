module Take_snapshot = struct
  type params = {
    directory: string;
    stack_name: string;
  }

  type result = unit

  let name = "file/take_snapshot"
end
