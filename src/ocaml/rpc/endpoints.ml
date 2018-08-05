(** This module provides endpoints for RPC methods. *)
module D = Sxfiler_domain
module Co = Sxfiler_completion.Domain

(** endpoints for Completion *)
module Completion = struct
  module Setup = struct
    type params = {
      source: Co.collection;
    }
    type result = unit
    let endpoint = "completion/setup"
  end

  module Read = struct
    type params = {
      input: string;
    }

    type result = Co.result
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

    type result = D.Scanner.t
    let endpoint = "scanner/make"
  end

  module Get = struct
    type params = {
      name: string;
    }

    type result = D.Scanner.t
    let endpoint = "scanner/get"
  end
end

(** endpoints for Configuration *)
module Configuration = struct
  module Get = struct
    type params = unit
    type result = D.Configuration.t
    let endpoint = "configuration/get"
  end
end

(** endpoints for Keymap *)
module Keymap = struct
  module Get = struct
    type params = unit
    type result = string D.Key_map.t
    let endpoint = "keymap/get"
  end
end
