import * as Common from "./jsonrpc";

// type of function to expose
type expose = (request: Common.Request) => Promise<any>;

// type of exposed functions in server
interface ExposedFunctions {
  [key: string]: expose;
}

// The class to handle request of JSON RPC
export class Server {
  private exposedFunctions: ExposedFunctions;
  constructor() {
    this.exposedFunctions = {};
  }

  /**
   *  handle request with exposed function. This function do not return any response, so this
   *  handler handles notification only.
   *
   *  @param request request of RPC
   *  @return promise
   */
  public handleRequest(request: Common.Request): Promise<any> {
    if (!this.exposedFunctions[request.method]) {
      return Promise.resolve();
    }

    return this.exposedFunctions[request.method](request);
  }

  /**
   *  expose the function for method.
   *
   *  @param method name of method
   *  @param f handler for method
   */
  public expose(method: string, f: expose) {
    if (method.length <= 0) {
      return;
    }

    this.exposedFunctions[method] = f;
  }
}
