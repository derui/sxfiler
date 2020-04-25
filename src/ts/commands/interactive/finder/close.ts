import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions as CompleterActions } from "@/modules/completer";
import { actions as KeymapActions } from "@/modules/keymap";
import { UIContext } from "@/types/ui-context";

const identifier = "interactive.finder.close";

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
    async execute(dispatcher: Dispatcher<Actions>, _: CommandState) {
      dispatcher.dispatch(KeymapActions.replaceContext([UIContext.OnFileTree]));
      dispatcher.dispatch(CompleterActions.close());
    },
  };
};
