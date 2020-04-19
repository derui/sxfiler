import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/keymap";
import { UIContext } from "@/types/ui-context";

const identifier = "internal.keymap.remove-context";

export type Payload = { context: UIContext };
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
    async execute(dispatcher: Dispatcher<Actions>, _: CommandState, payload: Payload) {
      dispatcher.dispatch(actions.removeContexts([payload.context]));
    },
  };
};
