import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { FileEvent, FileItemOrder } from "@/generated/filer_pb";
import { actions } from "@/modules/filer";

const identifier = "internal.filer.update-file-window";

export interface Payload {
  readonly fileListId: string;
  readonly events: FileEvent[];
  readonly itemOrders: FileItemOrder[];
}
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
    async execute(dispatcher: Dispatcher<Actions>, _: CommandState, payload: Payload) {
      dispatcher.dispatch(actions.updateFileWindow(payload.fileWindow, payload.side));
    },
  };
};
