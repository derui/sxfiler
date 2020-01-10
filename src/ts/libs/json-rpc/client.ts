import * as Common from "./type";

// P and Res only use to define api
export type Api<
  T extends string,
  P,
  Req extends { toJSON(): { [key: string]: any } },
  Res extends any = any,
  Result extends any = any
> = {
  method: T;

  /**
     Transformer for param of the method
   */
  parametersTransformer: (param: P) => Req;

  /**
     Transformer for result of the method
   */
  resultTransformer: (res: Res | undefined, error?: Common.RPCError) => Result;
};

export type Notification<T extends string, P extends {} = any> = {
  method: T;

  /**
     Transformer for param of the method
   */
  parametersTransformer: (param: P) => any;
};

export type Client<M extends string> = {
  /**
   * call a method with or without request.
   *
   * @param api API definition to call
   * @param params object for RPC
   * @return promise to handling result of RPC
   */
  call<P, Req extends { toJSON(): { [key: string]: any } }, Res, Result>(
    api: Api<M, P, Req, Res, Result>,
    params: P
  ): Promise<Result>;

  /**
   * send notification with or without request
   *
   * @param api API definition to call
   * @param params object for RPC
   * @return promise to handling result of RPC
   */
  notify<P>(api: Notification<M, P>, params: P): void;
};

/**
 * The client of JSON-RPC
 */
export class ClientImpl<M extends string> implements Client<M> {
  private readonly requester: Common.Requester;
  private readonly idGenerator: Common.IDGenerator;

  constructor(requester: Common.Requester, idGenerator: Common.IDGenerator) {
    this.requester = requester;
    this.idGenerator = idGenerator;
  }

  public async call<P, Req extends { toJSON(): { [key: string]: any } }, Res, Result>(
    api: Api<M, P, Req, Res, Result>,
    params: P
  ): Promise<Result> {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method: api.method,
      params: api.parametersTransformer(params).toJSON(),
      id: this.idGenerator(),
    };

    return this.requester.call(rpcRequest).then(req => {
      const error = req.error ? new Common.RPCError(req.error.message, req.error) : undefined;
      return api.resultTransformer(req.result, error);
    });
  }

  public notify<P>(api: Notification<M, P>, params: P): void {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method: api.method,
      params: api.parametersTransformer(params).toJSON(),
    };

    this.requester.notify(rpcRequest);
  }
}
