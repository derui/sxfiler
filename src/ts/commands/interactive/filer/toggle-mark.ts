import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { currentFocusingItemSelector, getCurrentSideInPb } from "@/modules/filer/selectors";
import * as Procs from "@/rpc/client-procedures";
import { ToggleMarkOfItemRequest } from "@/generated/filer_pb";

const identifier = "interactive.filer.toggle-mark";

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
      const currentItem = currentFocusingItemSelector(args.state.filer);
      const side = getCurrentSideInPb(args.state.filer);

      if (!currentItem) {
        return;
      }
      const request = new ToggleMarkOfItemRequest();
      request.setItemId(currentItem.item.getId());
      request.setSide(side);

      await args.clientResolver.rpcClient().use(Procs.Filer.toggleMark)(request);
    },
  };
};
