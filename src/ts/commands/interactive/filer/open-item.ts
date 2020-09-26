import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import * as S from "@/modules/filer/selectors";

const identifier = "interactive.filer.open-item";

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
      const currentFileItem = S.currentFocusingItemSelector(args.state.filer);

      if (!currentFileItem) {
        return;
      }

      args.clientResolver.appClient().openItem(currentFileItem.item);
    },
  };
};
