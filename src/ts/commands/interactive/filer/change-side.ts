import { Actions } from "@/modules";
import { CommandLike, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/filer";

const identifier = "interactive.filer.change-side";

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
    async execute(dispatcher: Dispatcher<Actions>) {
      dispatcher.dispatch(actions.changeSide());
    },
  };
};
