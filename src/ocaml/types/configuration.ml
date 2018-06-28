(** [Key_maps] defines key mappings that are used in each view. *)
module Key_maps = struct
  type t = {
    file_list: Key_map.t;
  }

  let default = {
    file_list = Key_map.empty;
  }
end

(** [Viewer] provides configuration for viewer. This configuration will not manage
    on server side.
*)
module Viewer = struct
  type t = {
    current_stack_name: string;
    stack_layout: Types.Layout.t;
    key_maps: Key_maps.t;
  }

  (** [default] will store all default configurations for Viewer configuration *)
  let default = {
    current_stack_name = "";
    stack_layout = Types.Layout.Side_by_side;
    key_maps = Key_maps.default;
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
