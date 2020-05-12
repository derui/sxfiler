import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { GetRequest } from "@/generated/keymap_pb";
import * as Procs from "@/rpc/client-procedures";
import { actions } from "@/modules/keymap";
import { actions as logEventActions, LogEventCreators } from "@/modules/log-event";
import * as winston from "winston";
import { Loggers } from "@/loggers";

const identifier = "interactive.keymap.reload";

export type Payload = undefined;
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
    async execute(dispatcher: Dispatcher<Actions>, args: CommandState) {
      const logger = winston.loggers.get(Loggers.COMMAND);
      const request = new GetRequest();

      // Call RPC and do not wait anything. So all of filer updates are coming as notificastion from server.
      const response = await args.clientResolver.rpcClient().use(Procs.Keymap.getKeymap)(request);
      const keymap = response.getKeymap();
      if (!keymap) {
        logger.warn("Do not get any keymap");
        return;
      }

      logger.debug(`Get keymap`);
      dispatcher.dispatch(actions.update(keymap));
      dispatcher.dispatch(logEventActions.send(LogEventCreators.createKeymapReload(new Date())));
    },
  };
};
