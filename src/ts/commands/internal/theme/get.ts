import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/theme";
import { GetRequest } from "@/generated/theme_pb";
import { Theme } from "@/rpc/client-procedures";

const identifier = "internal.theme.select";

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
      const res = await args.clientResolver.rpcClient().use(Theme.getTheme)(new GetRequest());

      const theme = res.getTheme();
      if (theme) {
        dispatcher.dispatch(actions.update(theme));
      }
    },
  };
};
