open Action_intf

module T = Sxfiler_types_yojson

module No_side_effect : No_side_effect = struct
  let take_snapshot ~directory = Lwt.return @@ T.Tree_snapshot.make ~directory ~nodes:[]
end

module Side_effect : Side_effect = struct end
