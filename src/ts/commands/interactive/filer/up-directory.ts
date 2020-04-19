import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { getCurrentSideInPb } from "@/modules/filer/selectors";
import * as procs from "@/rpc/client-procedures";
import { UpDirectoryRequest } from "@/generated/filer_pb";

const identifier = "interactive.filer.up-directory";

export type Payload = undefined;
export type Command = CommandLike<Payload>;

/**
 * command descriptor to resolve
 */
export const descriptor: CommandDescriptor<Payload> = Object.freeze({
  identifier,
  payloadSpec: undefined as any,
});

export const createCommand = (): Command => {
  return {
    identifier,
    async execute(_: Dispatcher<Actions>, args: CommandState) {
      const side = getCurrentSideInPb(args.state.filer);

      const request = new UpDirectoryRequest();
      request.setSide(side);
      await args.clientResolver.rpcClient().use(procs.Filer.upDirectory)(request);
    },
  };
};
