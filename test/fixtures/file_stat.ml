module D = Sxfiler_domain

let fixture ?(size = Int64.zero) ?(directory = false) ?(symlink = false) () =
  let size = if Int64.(compare size zero) = -1 then Int64.zero else size in
  let mode =
    {
      D.File_stat.owner = { readable = true; writable = true; executable = true };
      group = { readable = true; writable = true; executable = true };
      others = { readable = false; writable = false; executable = true };
    }
  in
  D.File_stat.make ~mode ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100) ~mtime:(Int64.of_int 1000)
    ~ctime:(Int64.of_int 10000) ~size ~is_directory:directory ~is_file:(not directory)
    ~is_symlink:symlink
