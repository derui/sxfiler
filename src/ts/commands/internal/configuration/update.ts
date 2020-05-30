import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { ItemKey } from "@/configurations/types";
import { UpdateRequest } from "@/generated/configuration_pb";
import * as Proc from "@/rpc/client-procedures";

const identifier = "internal.configuration.update";

export type Payload = { value: any; itemKey: ItemKey<any> };
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
      const request = new UpdateRequest();
      request.setKeyList(Array.from(payload.itemKey));
      request.setJsonValue(JSON.stringify(payload.value));
      await args.clientResolver.rpcClient().use(Proc.Configuration.updateConfiguration)(request);
    },
  };
};
