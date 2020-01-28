/**
 * This module provides client and procedure interface, and implementation.
 * Usage of this module is below.
 *
 * ```
 *  // create client (need create services before)
 *  const client = Client.create(services);
 *
 *  client.use(Procedures.Filer.initialize)(req)
 * ```
 */

import { RPCClient } from "@/libs/websocket-rpc/client";

/**
 * definition of procedure that is able to call via client.
 */
export type ProcedureDef<Req, Res> = (client: RPCClient, request: Req) => Promise<Res>;

/**
 * Client interface to get function for procedure
 */
export type Client = {
  /**
   * Make procedure caller with `ProcedureDef`
   */
  use<Req, Res>(procedureDef: ProcedureDef<Req, Res>): (req: Req) => Promise<Res>;
};

export const create = function create(client: RPCClient): Client {
  return {
    use<Req, Res>(def: ProcedureDef<Req, Res>) {
      return (req: Req) => def(client, req);
    },
  };
};
