import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { actions } from "@/modules/filer";
import { FileList } from "@/generated/filer_pb";

const identifier = "internal.filer.apply-file-list-event";

export type Payload = {
  fileListEventType: number;
  fileList: FileList;
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
    async execute(dispatcher: Dispatcher<Actions>, _: CommandState, payload: Payload) {
      dispatcher.dispatch(actions.applyFileListEvent(payload.fileListEventType, payload.fileList));
    },
  };
};
