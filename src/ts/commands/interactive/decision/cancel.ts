import { Actions } from "@/modules";
import { CommandLike, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/decision";
import { actions as keymapActions } from "@/modules/keymap";
import { UIContext } from "@/types/ui-context";

const identifier = "interactive.decision.cancel";

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
    async execute(dispatcher: Dispatcher<Actions>) {
      dispatcher.dispatch(actions.cancel());
      dispatcher.dispatch(keymapActions.replaceContext([UIContext.OnFileTree]));
    },
  };
};
