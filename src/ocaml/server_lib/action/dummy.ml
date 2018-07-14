open Action_intf

module T = Sxfiler_types_yojson

module No_side_effect : No_side_effect = struct
  let read_dir ~directory:_ = Lwt.return []
end

module Side_effect : Side_effect = struct end
