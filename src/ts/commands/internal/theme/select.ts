import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { selectTheme } from "@/modules/theme/selectors";
import { actions } from "@/modules/theme";

const identifier = "internal.theme.select";

export type Payload = { name: string };
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
    execute(dispatcher: Dispatcher<Actions>, args: CommandState, payload: Payload) {
      const theme = selectTheme(args.state.theme, payload.name);

      if (theme) {
        dispatcher.dispatch(actions.select(name));
      }

      return Promise.resolve();
    },
  };
};
