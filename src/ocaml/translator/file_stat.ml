open Sxfiler_core

module G = Sxfiler_generated
(** Define objects for JSON friendly *)

type error =
  | Invalid_capability
  | No_mode
  | Invalid_time       of string
  | Invalid_uid        of string
  | Invalid_gid        of string
  | Invalid_size       of string
  | Invalid_link_path  of string
[@@deriving eq, show]

type capability = G.Filer.Capability.t

type mode = G.Filer.Mode.t

type t = G.Filer.FileStat.t

module Domain = struct
  module D = Sxfiler_domain.File_stat

  let mode_of_domain (t : D.Mode.t) =
    {
      G.Filer.Mode.owner =
        Some
          {
            G.Filer.Capability.writable = t.D.Mode.owner.writable;
            readable = t.owner.readable;
            executable = t.owner.executable;
          };
      group = Some { writable = t.group.writable; readable = t.group.readable; executable = t.group.executable };
      others = Some { writable = t.others.writable; readable = t.others.readable; executable = t.others.executable };
    }

  let capability_to_domain (t : capability) =
    D.Capability.make ~writable:t.writable ~readable:t.readable ~executable:t.executable

  let mode_to_domain (t : mode) =
    let open Result.Infix in
    let* owner = Option.to_result ~none:Invalid_capability t.owner in
    let* group = Option.to_result ~none:Invalid_capability t.group in
    let* others = Option.to_result ~none:Invalid_capability t.others in
    D.Mode.make ~owner:(capability_to_domain owner) ~group:(capability_to_domain group)
      ~others:(capability_to_domain others)
    |> Result.ok

  let of_domain (t : D.t) =
    {
      G.Filer.FileStat.mode = mode_of_domain t.stat.mode |> Option.some;
      uid = D.Uid.value t.stat.uid;
      gid = D.Gid.value t.stat.gid;
      atime = Time.to_rfc3339 t.stat.atime;
      ctime = Time.to_rfc3339 t.stat.ctime;
      mtime = Time.to_rfc3339 t.stat.mtime;
      size = D.Size.value t.stat.size |> Int64.to_string;
      link_path = (match t.kind with D.Kind.Symlink v -> Path.to_string v | _ -> "");
      is_directory = (match t.kind with D.Kind.Directory -> true | _ -> false);
      is_file = (match t.kind with D.Kind.File -> true | _ -> false);
      is_symlink = (match t.kind with D.Kind.Symlink _ -> true | _ -> false);
    }

  let to_domain (t : t) =
    let open Result.Infix in
    let* mode = Option.to_result ~none:No_mode t.mode in
    let* mode = mode_to_domain mode in
    let* uid = match D.Uid.make t.uid with Ok v -> Ok v | Error v -> Error (Invalid_uid v) in
    let* gid = match D.Gid.make t.gid with Ok v -> Ok v | Error v -> Error (Invalid_gid v) in
    let* atime = Time.of_rfc3339 t.atime |> Option.to_result ~none:(Invalid_time t.atime) in
    let* ctime = Time.of_rfc3339 t.ctime |> Option.to_result ~none:(Invalid_time t.ctime) in
    let* mtime = Time.of_rfc3339 t.mtime |> Option.to_result ~none:(Invalid_time t.mtime) in
    let* size = Int64.of_string t.size |> D.Size.make |> Result.map_error (fun v -> Invalid_size v) in
    let stat = D.Stat.make ~mode ~uid ~gid ~atime ~ctime ~mtime ~size in
    let stat =
      if t.is_directory then D.make_directory stat |> Result.ok
      else if t.is_symlink then
        let* link_path =
          Path.of_string t.link_path |> Result.map_error (function Path.Empty_path -> Invalid_link_path t.link_path)
        in
        D.make_symlink ~stat ~link_path |> Result.ok
      else D.make_file stat |> Result.ok
    in
    stat
end

include Domain
