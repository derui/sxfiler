open Action_intf

module T = Sxfiler_domain_yojson

module No_side_effect : No_side_effect = struct
  let resolve_realpath path = path
  let read_dir ~directory:_ = Lwt.return []
end

module Side_effect : Side_effect = struct end
