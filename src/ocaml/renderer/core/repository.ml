module T = Sxfiler_types

include Repository_intf

module Scanner : Scanner = struct
  type t = {
    mutable subscribers: (T.Scanner.t -> unit) list;
    table: T.Scanner.t Jstable.t;
  }

  let make () = {
    subscribers = [];
    table = Jstable.create ();
  }

  let get t name =
    let open Sxfiler_core.Option in
    get_exn @@ Js.Optdef.to_option @@ Jstable.find t.table Js.(string name)

  let store t scanner =
    let name = scanner.T.Scanner.location in
    Jstable.add t.table Js.(string name) scanner;
    List.iter (fun f -> f scanner) t.subscribers

  let on_change t f =
    t.subscribers <- f :: t.subscribers

end
