import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { getCurrentSideInPb, currentFocusingItemSelector, currentSideMarkedItems } from "@/modules/filer/selectors";
import { DeleteRequest, Target } from "@/generated/filer_pb";
import { Loggers } from "@/loggers";
import { loggers } from "winston";
import * as Procs from "@/rpc/client-procedures";
import { LogEventCreators, actions } from "@/modules/log-event";

const identifier = "interactive.filer.delete-items";

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
    async execute(dispatcher: Dispatcher<Actions>, args: CommandState) {
      const { filer } = args.state;

      const markedItems = currentSideMarkedItems(filer);
      const side = getCurrentSideInPb(filer);
      const request = new DeleteRequest();

      request.setSide(side);

      if (markedItems.length === 0) {
        const focused = currentFocusingItemSelector(filer);

        if (!focused) {
          loggers.get(Loggers.COMMAND).warn("Did not initialize filer before copy.");
          return;
        }
        request.setTarget(Target.ONE);
        request.setTargetId(focused.item.getId());
      } else {
        request.setTarget(Target.MARKED);
      }

      const response = await args.clientResolver.rpcClient().use(Procs.Filer.deleteItems)(request);
      const events = response.getResultsList().map((v) => {
        return LogEventCreators.createDeleteItem(new Date(v.getTimestamp()), v.getPath());
      });
      dispatcher.dispatch(actions.send(events));
    },
  };
};
