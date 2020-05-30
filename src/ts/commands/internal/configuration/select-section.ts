import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/configuration";
import { Section } from "@/configurations/types";

const identifier = "internal.configuration.select-section";

export type Payload = {
  section: Section;
};
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
      dispatcher.dispatch(actions.selectSection(payload.section));
    },
  };
};
