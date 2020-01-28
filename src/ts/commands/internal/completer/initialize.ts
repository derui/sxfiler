import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { Item, InitializeRequest } from "@/generated/completer_pb";
import * as Proc from "@/rpc/client-procedures";

const identifier = "internal.completer.initialize";

export type Payload = { source: Item[] };
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
      const request = new InitializeRequest();
      request.setSourceList(payload.source);
      await args.clientResolver.rpcClient().use(Proc.Completer.initialize)(request);
    },
  };
};
