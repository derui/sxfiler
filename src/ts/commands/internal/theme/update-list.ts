import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { Theme } from "@/rpc/client-procedures";
import { actions } from "@/modules/theme";
import { ListRequest } from "@/generated/theme_pb";

const identifier = "internal.theme.update-list";

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
      const req = new ListRequest();
      const res = await args.clientResolver.rpcClient().use(Theme.list)(req);

      dispatcher.dispatch(actions.updateList(res.getThemesList()));
    },
  };
};
