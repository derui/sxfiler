import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { DecisionRequiredOp } from "@/modules/decision/reducer";
import { FileItem } from "@/generated/filer_pb";
import { actions } from "@/modules/decision";

const identifier = "internal.decision.request";

export type Payload = {
  requiredOp: DecisionRequiredOp;
  fileItem: FileItem;
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
      const { fileItem, requiredOp } = payload;
      switch (requiredOp) {
        case DecisionRequiredOp.Copy:
          return dispatcher.dispatch(actions.requireDecisionForCopy("copy", fileItem));
        case DecisionRequiredOp.Move:
          return dispatcher.dispatch(actions.requireDecisionForMove("move", fileItem));
        case DecisionRequiredOp.Delete:
          return dispatcher.dispatch(actions.requireDecisionForDelete("delete", fileItem));
        default:
          return;
      }
    },
  };
};
