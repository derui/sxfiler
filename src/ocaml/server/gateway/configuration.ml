open Sxfiler_core
module Usecase = Sxfiler_usecase
module T = Sxfiler_server_translator
module G = Sxfiler_server_generated.Configuration

(** The gateway for Use Case of {!Rpc.Configuration.Get} *)
module Get = struct
  module Type = struct
    type input = G.GetRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = G.GetResponse.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Configuration.Get.S) : S = struct
    include Type

    let handle () =
      match%lwt Usecase.execute () with
      | Ok output ->
          Lwt.return_ok
            { G.GetResponse.configuration = T.Configuration.of_domain output |> Option.some }
      | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
  end
end

(** The gateway for use case of {!Rpc.Configuration.Store} *)
module Store = struct
  module Type = struct
    type input = G.StoreRequest.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
    type output = G.StoreResponse.t [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  end

  module type S = Core.Gateway with type input = Type.input and type output = Type.output

  module Make (Usecase : Usecase.Configuration.Store.S) : S = struct
    include Type

    let handle (input : G.StoreRequest.t) =
      match input.configuration with
      | None -> Lwt.return_error Gateway_error.(Unknown_error "unknown error")
      | Some input -> (
          match%lwt Usecase.execute @@ T.Configuration.to_domain input with
          | Ok () -> Lwt.return_ok ()
          | Error () -> Lwt.return_error Gateway_error.(Unknown_error "unknown error") )
  end
end
