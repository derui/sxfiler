import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import * as Proc from "@/rpc/client-procedures";
import { CompleteRequest } from "@/generated/completer_pb";

const identifier = "internal.completer.complete";

export type Payload = { input: string };
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
    async execute(_: Dispatcher<Actions>, args: CommandState, payload: Payload) {
      const request = new CompleteRequest();
      request.setInput(payload.input);
      await args.clientResolver.rpcClient().use(Proc.Completer.complete)(request);
    },
  };
};
