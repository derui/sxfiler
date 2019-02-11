import * as Common from "./type";

// P and Res only use to define api
export interface Api<T extends string, P extends {} = {}, Res extends {} = {}> {
  method: T;
}

export interface Client<M extends string> {
  call<P, Res>(api: Api<M, P, Res>, params?: P): Promise<Res>;
  notify<P>(api: Api<M, P, any>, params?: P): void;
}

/**
 * The client of JSON-RPC
 */
export class ClientImpl<M extends string> implements Client<M> {
  constructor(private readonly requester: Common.Requester, private readonly idGenerator: Common.IDGenerator) {}

  /**
   * call a method with or without request.
   *
   * @param api API definition to call
   * @param params object for RPC
   * @return promise to handling result of RPC
   */
  public async call<P, Res>(api: Api<M, P, Res>, params?: P): Promise<Res> {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method: api.method,
      params,
      id: this.idGenerator(),
    };

    return this.requester.call(rpcRequest).then(req => {
      if (req.error) {
        return Promise.reject(new Common.RPCError(req.error.message, req.error));
      }
      return req.result;
    });
  }

  public notify<P>(api: Api<M, P, any>, params?: P): void {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method: api.method,
      params,
    };

    this.requester.notify(rpcRequest);
  }
}
