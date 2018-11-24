import * as Common from "./jsonrpc";

/**
 * The client of JSON-RPC
 */
export class Client {
  constructor(private readonly requester: Common.Requester, private readonly idGenerator: Common.IDGenerator) {}

  /**
   * call a method with or without request.
   *
   * @param method name of RPC
   * @param request object for RPC
   * @return promise to handling result of RPC
   */
  public call<Req, Res>(method: string, request?: Req): Promise<Res> {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method,
      params: request,
      id: this.idGenerator(),
    };

    return this.requester.call(rpcRequest).then(req => {
      if (req.error) {
        return Promise.reject(new Common.RPCError(req.error.message, req.error));
      }
      return req.result;
    });
  }

  public notify<Req>(method: string, request?: Req): void {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method,
      params: request,
    };

    return this.requester.notify(rpcRequest);
  }
}
