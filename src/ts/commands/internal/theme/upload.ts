import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { Loggers } from "@/loggers";
import winston from "winston";
import { actions } from "@/modules/theme";
import { Theme } from "@/rpc/client-procedures";
import { AddRequest, ColorCode, ListRequest } from "@/generated/theme_pb";

const identifier = "internal.theme.upload";

export type Payload = { themeJson: string };
export type Command = CommandLike<Payload>;

/**
 * command descriptor to resolve
 */
export const descriptor: CommandDescriptor<Payload> = Object.freeze({
  identifier,
  payloadSpec: undefined as any,
});

const requiredKeys = ["name", "colors"];
const logger = winston.loggers.get(Loggers.COMMAND);

export const createCommand = (): Command => {
  return {
    identifier,
    async execute(dispatcher: Dispatcher<Actions>, args: CommandState, payload: Payload) {
      const obj = JSON.parse(payload.themeJson);
      const keys = new Set(Object.keys(obj));

      if (!requiredKeys.some((v) => !keys.has(v))) {
        logger.warn(`Theme does not contains required keys: ${requiredKeys}`);
        actions.invalidTheme([`Theme does not contains required keys: ${requiredKeys}`]);
        return;
      }

      if (!(obj.colors instanceof Object)) {
        actions.invalidTheme([`Colors must be object`]);
        return;
      }

      const colors: { [p: string]: string } = obj.colors;

      const req = new AddRequest();
      req.setName(obj.name);
      req.setDescription(obj.description || null);
      req.setColorCodesList(
        Object.entries(colors).map(([key, v]) => {
          const code = new ColorCode();
          code.setName(key);
          code.setHexColor(v);
          return code;
        })
      );

      await args.clientResolver.rpcClient().use(Theme.add)(req);
      const res = await args.clientResolver.rpcClient().use(Theme.list)(new ListRequest());
      dispatcher.dispatch(actions.updateList(res.getThemesList()));
    },
  };
};
