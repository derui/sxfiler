import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { MoveLocationRequest } from "@/generated/filer_pb";
import { currentFocusingItemSelector, getCurrentSideInPb } from "@/modules/filer/selectors";
import { Filer } from "@/rpc/client-procedures";

const identifier = "interactive.filer.move-location";

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
    async execute(_: Dispatcher<Actions>, args: CommandState) {
      const request = new MoveLocationRequest();

      const focused = currentFocusingItemSelector(args.state.filer);
      if (!focused) {
        return;
      }
      const { item } = focused;
      if (!(item.getStat()?.getIsDirectory() || false)) {
        return;
      }

      request.setLocation(item.getFullPath());
      request.setSide(getCurrentSideInPb(args.state.filer));

      await args.clientResolver.rpcClient().use(Filer.moveLocation)(request);
    },
  };
};
