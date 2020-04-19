open Sxfiler_core
module D = Sxfiler_domain
module L = Sxfiler_domain.Location_history
module G = Sxfiler_generated

module File_window = struct
  let of_domain (t : 'a D.File_window.t) =
    {
      G.Filer.FileWindow.file_list = File_list.of_domain t.file_list |> Option.some;
      history = Location_history.of_domain t.history |> Option.some;
    }
end

let of_domain (t : D.Filer.t) =
  {
    G.Filer.Filer.left_file_window = File_window.of_domain t.left_file_window |> Option.some;
    right_file_window = File_window.of_domain t.right_file_window |> Option.some;
  }
