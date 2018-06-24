(** [Key_maps] defines key mappings that are used in each view. *)
module Key_maps = struct
  type t = {
    file_list: Key_map.t;
  }

  let empty = {
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

  let make ~current_stack_name
      ~stack_layout
      ~key_maps =
    {
      current_stack_name; stack_layout; key_maps
    }

end

(** [Server] provides configuration for server. This configuration will not manage
    on viewer side.
*)
module Server = struct
  type t = {
    sort_order: Types.Sort_type.t;
  }

  let make ~sort_order = {sort_order}
end
