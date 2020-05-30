import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { Configuration } from "@/generated/configuration_pb";
import { actions } from "@/modules/configuration";

const identifier = "internal.configuration.update-all";

export type Payload = { configurations: Configuration[] };
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
      dispatcher.dispatch(actions.update(payload.configurations));
    },
  };
};
