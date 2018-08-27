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

(** endpoints for Filer *)
module Filer = struct
  module Make = struct
    type params = {
      initial_location: string;
      name: string;
    }

    type result = Types.Filer.t
    let endpoint = "filer/make"
  end

  module Get = struct
    type params = {
      name: string;
    }

    type result = Types.Filer.t
    let endpoint = "filer/get"
  end

  module Move_parent = struct
    type params = {
      name: string;
      new_location: string;
    }

    type result = Types.Filer.t
    let endpoint = "filer/move"
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

  module Enable_context = struct
    type params = {
      context: string;
    }
    type result = Types.Key_map.t
    let endpoint = "keymap/enableContext"
  end

  module Disable_context = struct
    type params = {
      context: string;
    }
    type result = Types.Key_map.t
    let endpoint = "keymap/disbleContext"
  end
end
