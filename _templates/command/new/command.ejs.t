---
to: src/ts/commands/<%= group %>/<%= module %>/<%= name %>.ts
---
import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";

const identifier = "<%= group %>.<%= module %>.<%= name %>";

<% if (interactive) { -%>
export type Payload = undefined;
export type Command = CommandLike<Payload>;
<% } else { -%>
export type Payload = {};
export type Command = CommandLike<Payload>;
<% } -%>

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
    },
  };
};
