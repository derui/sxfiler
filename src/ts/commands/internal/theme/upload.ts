import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/theme";
import { Theme } from "@/rpc/client-procedures";
import { UpdateRequest, ColorPair } from "@/generated/theme_pb";

const identifier = "internal.theme.upload";

export type Payload = { baseTheme?: string; colors: { [p: string]: string } };
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
    async execute(dispatcher: Dispatcher<Actions>, args: CommandState, payload: Payload) {
      const req = new UpdateRequest();
      req.setBaseTheme(payload.baseTheme || "");

      Object.entries(payload.colors).forEach(([key, value]) => {
        const pair = new ColorPair();
        pair.setName(key);
        pair.setHexColor(value);
        req.addColorPairs(pair);
      });

      const res = await args.clientResolver.rpcClient().use(Theme.update)(req);
      const theme = res.getTheme();
      if (theme) {
        dispatcher.dispatch(actions.update(theme));
      }
    },
  };
};
