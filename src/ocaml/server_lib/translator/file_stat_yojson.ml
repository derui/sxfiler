module T : Core.Translator with type t = File_stat.t and type target = Yojson.Safe.t = struct
  type t = File_stat.t
  type target = Yojson.Safe.t

  let of_target = File_stat.of_yojson
  let to_target = File_stat.to_yojson
end

include T
