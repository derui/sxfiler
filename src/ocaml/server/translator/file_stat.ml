open Sxfiler_core

module G = Sxfiler_server_generated
(** Define objects for JSON friendly *)

type capability = G.Filer.Capability.t
type mode = G.Filer.Mode.t
type t = G.Filer.FileStat.t

module Domain = struct
  module D = Sxfiler_domain.File_stat

  let mode_of_domain (t : D.mode) =
    {
      G.Filer.Mode.owner =
        Some
          {
            G.Filer.Capability.writable = t.D.owner.writable;
            readable = t.D.owner.readable;
            executable = t.D.owner.executable;
          };
      group =
        Some
          {
            writable = t.D.group.writable;
            readable = t.D.group.readable;
            executable = t.D.group.executable;
          };
      others =
        Some
          {
            writable = t.D.others.writable;
            readable = t.D.others.readable;
            executable = t.D.others.executable;
          };
    }

  let capability_to_domain (t : capability) =
    { D.writable = t.writable; readable = t.readable; executable = t.executable }

  let mode_to_domain (t : mode) =
    {
      D.owner = Option.get_exn t.owner |> capability_to_domain;
      group = Option.get_exn t.group |> capability_to_domain;
      others = Option.get_exn t.others |> capability_to_domain;
    }

  let of_domain (t : D.t) =
    {
      G.Filer.FileStat.mode = mode_of_domain t.mode |> Option.some;
      uid = t.uid;
      gid = t.gid;
      atime = Int64.to_string t.atime;
      ctime = Int64.to_string t.ctime;
      mtime = Int64.to_string t.mtime;
      size = Int64.to_string t.size;
      isDirectory = t.is_directory;
      isFile = t.is_file;
      isSymlink = t.is_symlink;
    }

  let to_domain (t : t) =
    {
      D.mode = Option.get_exn t.mode |> mode_to_domain;
      uid = t.uid;
      gid = t.gid;
      atime = Int64.of_string t.atime;
      ctime = Int64.of_string t.ctime;
      mtime = Int64.of_string t.mtime;
      size = Int64.of_string t.size;
      is_directory = t.isDirectory;
      is_file = t.isFile;
      is_symlink = t.isSymlink;
    }
end

include Domain
