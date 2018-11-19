import * as Common from "./jsonrpc";

// interface to request to server.
// first argument is method of JSON-RPC
type Requester = (request: Common.Request) => Promise<Common.Response>;

// Simple interface for Generator for request
type IDGenerator = () => string;

/**
 * The client of JSON-RPC
 */
export class Client {
  constructor(private readonly requester: Requester, private readonly idGenerator: IDGenerator) {}

  /**
   * send a request to method .
   *
   * @param method name of RPC
   * @param request object for RPC
   * @return promise to handling result of RPC
   */
  public send<Req, Res>(method: string, request?: Req): Promise<Res> {
    const rpcRequest: Common.Request = {
      jsonrpc: "2.0",
      method,
      params: request,
      id: this.idGenerator(),
    };

    return this.requester(rpcRequest).then(req => {
      if (req.error) {
        return Promise.reject(new Common.RPCError(req.error.message, req.error));
      }
      return req.result;
    });
  }
}
