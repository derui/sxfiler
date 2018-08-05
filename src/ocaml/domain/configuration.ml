(** [Viewer] provides configuration for viewer. This configuration will not manage
    on server side.
*)
module Viewer = struct
  type t = {
    current_stack_name: string;
    stack_layout: Types.Layout.t;
  }

  (** [default] will store all default configurations for Viewer configuration *)
  let default = {
    current_stack_name = "";
    stack_layout = Types.Layout.Side_by_side;
  }

end

(** [Server] provides configuration for server. This configuration will not manage
    on viewer side.
*)
module Server = struct
  type t = {
    sort_order: Types.Sort_type.t;
  }

  let default = {
    sort_order = Types.Sort_type.Name;
  }
end

(** Total configuration *)
type t = {
  viewer: Viewer.t;
  server: Server.t;
}

let default = {
  viewer = Viewer.default;
  server = Server.default;
}

module type Repository = sig
  (** [resolve ()] returns configuration. Configuration should be singleton.  *)
  val resolve: unit -> t Lwt.t

  (** [store t] saves the [t] as singleton *)
  val store: t -> unit Lwt.t
end
