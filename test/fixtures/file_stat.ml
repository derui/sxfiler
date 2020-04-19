open Sxfiler_domain
module C = Sxfiler_core

let random_state = Random.State.make_self_init ()

let capability () =
  File_stat.Capability.make ~writable:(Random.State.bool random_state) ~readable:(Random.State.bool random_state)
    ~executable:(Random.State.bool random_state)

let mode_fixture () = File_stat.Mode.make ~owner:(capability ()) ~group:(capability ()) ~others:(capability ())

let uid_fixture () = File_stat.Uid.make (Random.State.int random_state 10000) |> C.Result.to_option |> Option.get

let gid_fixture () = File_stat.Gid.make (Random.State.int random_state 10000) |> C.Result.to_option |> Option.get

let time_fixture () = Random.State.float random_state 10000. |> C.Time.of_float |> Option.get

let size_fixture () = Random.State.int64 random_state 100000L |> File_stat.Size.make |> C.Result.to_option |> Option.get

let stat () =
  File_stat.Stat.make ~mode:(mode_fixture ()) ~uid:(uid_fixture ()) ~gid:(gid_fixture ()) ~atime:(time_fixture ())
    ~mtime:(time_fixture ()) ~ctime:(time_fixture ()) ~size:(size_fixture ())

let file_fixture () = stat () |> File_stat.make_file

let directory_fixture () = stat () |> File_stat.make_directory

let symlink_fixture link_path = File_stat.make_symlink ~stat:(stat ()) ~link_path
