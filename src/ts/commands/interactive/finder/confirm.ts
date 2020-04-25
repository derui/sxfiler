import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { selectCurrentFocusedItem } from "@/modules/completer/selectors";
import * as Completer from "@/modules/completer";
import * as Keymap from "@/modules/keymap";
import * as Filer from "@/modules/filer";
import { UIContext } from "@/types/ui-context";

const identifier = "interactive.finder.confirm";

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
      const currentItem = selectCurrentFocusedItem(args.state.completer);

      const id = currentItem?.value?.id;
      if (!id) {
        return;
      }

      dispatcher.dispatch(Completer.actions.close());
      dispatcher.dispatch(Keymap.actions.replaceContext([UIContext.OnFileTree]));
      dispatcher.dispatch(Filer.actions.focusItem(id));
    },
  };
};
