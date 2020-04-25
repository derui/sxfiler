import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { Candidate } from "@/generated/completer_pb";
import * as C from "@/modules/completer";

const identifier = "internal.completer.update-candidates";

export type Payload = { candidates: Candidate[] };
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
    async execute(dispatcher: Dispatcher<Actions>, _: CommandState, payload: Payload) {
      dispatcher.dispatch(C.actions.updateCandidates(payload.candidates));
    },
  };
};
