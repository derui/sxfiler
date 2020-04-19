open Abbrev

(* query endpoints *)
type get = F.Common_step.Configuration.load -> Endpoint.t

let get : get =
 fun load ->
  Endpoint.with_request (G.Configuration.GetRequest.from_proto, G.Configuration.GetResponse.to_proto) ~f:(fun () ->
      let%lwt configuration = load () in
      Lwt.return_ok
        ({ G.Configuration.GetResponse.configuration = Tr.Configuration.of_domain configuration |> Option.some }, []))
