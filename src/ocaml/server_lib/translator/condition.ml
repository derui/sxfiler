module D = Sxfiler_domain.Condition

type t = {
  on_file_tree: bool [@key "onFileTree"][@default false];
  on_completing: bool [@key "onCompleting"][@default false];
}
[@@deriving yojson,show]

let of_domain t =
  let module T = Sxfiler_domain in
  let list = T.Condition.to_list t in
  {
    on_file_tree = List.mem T.Condition.On_file_tree list;
    on_completing = List.mem T.Condition.On_completing list;
  }

let to_domain t =
  let empty = D.empty in
  List.fold_left (fun cond (context, enabled) ->
      if enabled then D.enable cond ~context
      else cond
    ) empty
    [
      D.On_file_tree, t.on_file_tree;
      D.On_completing, t.on_completing;
    ]
