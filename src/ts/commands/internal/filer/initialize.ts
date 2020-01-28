import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { InitializeRequest } from "@/generated/filer_pb";
import * as Procs from "@/rpc/client-procedures";

const identifier = "internal.filer.initialize";

export type Payload = { leftLocation: string; rightLocation: string };
export type Command = CommandLike<Payload>;

/**
 * command descriptor to resolve
 */
export const descriptor: CommandDescriptor<Payload> = {
  identifier,
  payloadSpec: undefined as any,
};

export const createCommand = function createCommand(): Command {
  return {
    identifier,
    async execute(_: Dispatcher<Actions>, args: CommandState, payload: Payload) {
      const request = new InitializeRequest();
      request.setLeftLocation(payload.leftLocation);
      request.setRightLocation(payload.rightLocation);

      // Call RPC and do not wait anything. So all of filer updates are coming as notificastion from server.
      await args.clientResolver.rpcClient().use(Procs.Filer.initialize)(request);
    },
  };
};
