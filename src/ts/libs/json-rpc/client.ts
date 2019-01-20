import * as Common from "./type";

export interface Request<M extends string, P extends {} = {}> {
  method: M;
  params?: P;
}

export interface Client<M extends string> {
  call<P, Res>(request: Request<M, P>): Promise<Res>;
  notify<P>(request: Request<M, P>): void;
}

/**
 * The client of JSON-RPC
 */
export class ClientImpl<M extends string> implements Client<M> {
  constructor(private readonly requester: Common.Requester, private readonly idGenerator: Common.IDGenerator) {}

  /**
   * call a method with or without request.
   *
   * @param method name of RPC
   * @param request object for RPC
   * @return promise to handling result of RPC
   */
  public async call<P, Res>(request: Request<M, P>): Promise<Res> {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method: request.method,
      params: request.params,
      id: this.idGenerator(),
    };

    return this.requester.call(rpcRequest).then(req => {
      if (req.error) {
        return Promise.reject(new Common.RPCError(req.error.message, req.error));
      }
      return req.result;
    });
  }

  public notify<P>(request: Request<M, P>): void {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method: request.method,
      params: request.params,
    };

    this.requester.notify(rpcRequest);
  }
}
