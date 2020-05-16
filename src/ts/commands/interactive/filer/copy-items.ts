import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { currentFocusingItemSelector, currentSideMarkedItems, getCurrentSideInPb } from "@/modules/filer/selectors";
import { CopyRequest, Side, Direction, Target, Transfer, TransferStatus } from "@/generated/filer_pb";
import { Loggers } from "@/loggers";
import { loggers } from "winston";
import * as Procs from "@/rpc/client-procedures";
import { LogEventCreators, actions } from "@/modules/log-event";

const identifier = "interactive.filer.copy-items";

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
      const request = new CopyRequest();
      const transfer = new Transfer();

      switch (side) {
        case Side.LEFT:
          transfer.setDirection(Direction.LEFT_TO_RIGHT);
          break;
        case Side.RIGHT:
          transfer.setDirection(Direction.RIGHT_TO_LEFT);
          break;
      }

      if (markedItems.length === 0) {
        const focused = currentFocusingItemSelector(filer);

        if (!focused) {
          loggers.get(Loggers.COMMAND).warn("Did not initialize filer before copy.");
          return;
        }
        transfer.setTarget(Target.ONE);
        transfer.setTargetId(focused.item.getId());
      } else {
        transfer.setTarget(Target.MARKED);
      }
      request.setTransfer(transfer);

      const response = await args.clientResolver.rpcClient().use(Procs.Filer.copyItems)(request);
      const events = response
        .getResultsList()
        .filter((v) => v.getStatus() === TransferStatus.SUCCESS)
        .map((v) => {
          return LogEventCreators.createCopyItem(new Date(v.getTimestamp()), v.getSource(), v.getDestination());
        });
      dispatcher.dispatch(actions.send(events));
    },
  };
};
