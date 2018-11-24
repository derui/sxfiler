// the interface of request of JSON-RPC
export interface Request {
  jsonrpc: "2.0";
  method: string;
  params?: any;
  id?: string;
}

// Error message
export interface Error {
  code: number;
  message: string;
  data?: any;
}

export interface Response {
  jsonrpc: "2.0";
  result?: any;
  error?: Error;
  id?: string;
}

// Extended error to handle JSONRPC error.
export class RPCError extends Error {
  // Error object of RPC
  public readonly error: Error;
  constructor(m: string, error: Error) {
    super(m);

    Object.setPrototypeOf(this, RPCError.prototype);
    this.error = error;
  }
}

// interface to request to server.
export interface Requester {
  /**
   * call a method of server
   * @param request request object
   */
  call(request: Request): Promise<Response>;

  /**
   * send notification to server.
   * @param request request object
   */
  notify(request: Request): void;
}

// Simple interface for Generator for request
export type IDGenerator = () => string;
