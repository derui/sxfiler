import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import * as Procs from "@/rpc/client-procedures";
import { GetRequest } from "@/generated/configuration_pb";
import { actions } from "@/modules/configuration";
import * as winston from "winston";
import { Loggers } from "@/loggers";

const identifier = "internal.configuration.initialize";

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
      const res = await args.clientResolver.rpcClient().use(Procs.Configuration.getConfiguration)(request);

      const configuration = res.getConfiguration();
      if (!configuration) {
        logger.warn("Do not get configuration");
        return;
      }

      dispatcher.dispatch(actions.update(configuration));
    },
  };
};
