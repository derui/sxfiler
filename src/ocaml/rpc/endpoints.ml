(** This module provides endpoints for RPC methods. *)

(** endpoints for Completion *)
module Completion = struct
  module Setup = struct
    type params = {
      source: Types.Completion.Item.t list;
    }
    type result = unit
    let endpoint = "completion/setup"
  end

  module Read = struct
    type params = {
      input: string;
    }

    type result = Types.Completion.Candidate.t list
    let endpoint = "completion/read"
  end
end

(** endpoints for Scanner *)
module Scanner = struct
  module Make = struct
    type params = {
      initial_location: string;
      name: string;
    }

    type result = Types.Scanner.t
    let endpoint = "scanner/make"
  end

  module Get = struct
    type params = {
      name: string;
    }

    type result = Types.Scanner.t
    let endpoint = "scanner/get"
  end
end

(** endpoints for Configuration *)
module Configuration = struct
  module Get = struct
    type params = unit
    type result = Types.Configuration.t
    let endpoint = "configuration/get"
  end
end

(** endpoints for Keymap *)
module Keymap = struct
  module Get = struct
    type params = unit
    type result = Types.Key_map.t
    let endpoint = "keymap/get"
  end
end

(** endpoints for condition *)
module Condition = struct
  module Enable = struct
    type params = {
      context: string;
    }
    type result = unit
    let endpoint = "condition/enable"
  end

  module Disable = struct
    type params = {
      context: string;
    }
    type result = unit
    let endpoint = "condition/disble"
  end
end
