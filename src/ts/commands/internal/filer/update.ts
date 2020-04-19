import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { Filer } from "@/generated/filer_pb";
import { actions } from "@/modules/filer";

const identifier = "internal.filer.update";

export type Payload = { filer?: Filer };
export type Command = CommandLike<{}>;

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
    async execute(dispatcher: Dispatcher<Actions>, _: CommandState, payload: Payload) {
      const filer = payload.filer;
      if (!filer) {
        return;
      }

      dispatcher.dispatch(actions.update(filer));
    },
  };
};
