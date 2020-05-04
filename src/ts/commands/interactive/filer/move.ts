import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { currentFocusingItemSelector, currentSideMarkedItems, getCurrentSideInPb } from "@/modules/filer/selectors";
import { MoveRequest, Side, Direction, Target, Transfer } from "@/generated/filer_pb";
import { Loggers } from "@/loggers";
import { loggers } from "winston";
import * as Procs from "@/rpc/client-procedures";

const identifier = "interactive.filer.move";

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
      const { filer } = args.state;

      const markedItems = currentSideMarkedItems(filer);
      const side = getCurrentSideInPb(filer);
      const request = new MoveRequest();
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
          loggers.get(Loggers.COMMAND).warn("Did not initialize filer before move.");
          return;
        }
        transfer.setTarget(Target.ONE);
        transfer.setTargetId(focused.item.getId());
      } else {
        transfer.setTarget(Target.MARKED);
      }
      request.setTransfer(transfer);

      await args.clientResolver.rpcClient().use(Procs.Filer.move)(request);
    },
  };
};
