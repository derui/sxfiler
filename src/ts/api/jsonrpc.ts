
namespace JsonRPC {

  // the interface of request of JSON-RPC
  interface Request {
    jsonrpc: '2.0';
    method: string;
    params?: any;
    id?: string;
  }

  // Error message
  interface Error {
    code: number;
    message: string;
    data?: any;
  }

  interface Response {
    jsonrpc: '2.0';
    result?: any;
    error?: Error;
    id?: string;
  }

  // interface to request to server.
  interface Requester {
    // first argument is method of JSON-RPC
    (request:Request): Promise<Response>;
  }

  // interface to send response to client.
  interface Responder {
    (response:Response): Promise<void>;
  }


  // Simple interface for Generator for request
  type IDGenerator = () => string;

  // Extended error to handle JSONRPC error.
  class RPCError extends Error {

    // Error object of RPC
    readonly error : Error;
    constructor(m:string, error:Error) {
      super(m);

      Object.setPrototypeOf(this, RPCError.prototype);
      this.error = error;
    }
  }

  /**
     The client of JSON-RPC
   */
  class Client {
    constructor(private readonly requester: Requester,
                private readonly idGenerator: IDGenerator) {}

    /**
       send a request to method .

       @param method name of RPC
       @param request object for RPC
       @return promise to handling result of RPC
     */
    send<Req, Res>(method:string, request?:Req): Promise<Res> {
      const rpcRequest: Request = {
        jsonrpc: '2.0',
        method,
        params: request,
        id: this.idGenerator()
      };

      return this.requester(rpcRequest).then(req => {
        if (req.error) {
          return Promise.reject(new RPCError(req.error.message, req.error));
        }
        return req.result
      });
    }
  }

  // The class to handle request of JSON RPC
  class Server {
  }
}
