import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { ReloadAllRequest } from "@/generated/filer_pb";
import * as P from "@/rpc/client-procedures";

const identifier = "interactive.filer.reload-all";

export type Payload = undefined;
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
    async execute(_: Dispatcher<Actions>, args: CommandState, payload) {
      const request = new ReloadAllRequest();
      await args.clientResolver.rpcClient().use(P.Filer.reloadAll)(request);
    },
  };
};
