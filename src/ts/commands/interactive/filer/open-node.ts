import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { currentFocusingItemSelector } from "@/modules/filer/selectors";
import { OpenFileItemRequest } from "@/generated/filer_pb";
import * as procs from "@/rpc/client-procedures";

const identifier = "interactive.filer.open-node";

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
      const focused = currentFocusingItemSelector(args.state.filer);
      if (!focused) {
        return;
      }

      const { item, side } = focused;
      const request = new OpenFileItemRequest();
      request.setSide(side);
      request.setFileItemId(item.getId());
      await args.clientResolver.rpcClient().use(procs.Filer.openFileItem)(request);
    },
  };
};
