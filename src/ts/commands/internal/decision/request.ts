import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { DecisionRequiredOp } from "@/modules/decision/reducer";
import { FileItem } from "@/generated/filer_pb";
import { actions } from "@/modules/decision";
import { actions as keymapActions } from "@/modules/keymap";
import { UIContext } from "@/types/ui-context";

const identifier = "internal.decision.request";

export type Payload = {
  requiredOp: DecisionRequiredOp;
  processId: string;
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
      const { fileItem, requiredOp, processId } = payload;
      switch (requiredOp) {
        case DecisionRequiredOp.Copy:
          dispatcher.dispatch(actions.requireDecisionForCopy(processId, fileItem));
          dispatcher.dispatch(keymapActions.replaceContext([UIContext.OnDecision]));
          break;
        case DecisionRequiredOp.Move:
          dispatcher.dispatch(actions.requireDecisionForMove(processId, fileItem));
          dispatcher.dispatch(keymapActions.replaceContext([UIContext.OnDecision]));
          break;
        case DecisionRequiredOp.Delete:
          dispatcher.dispatch(actions.requireDecisionForDelete(processId, fileItem));
          dispatcher.dispatch(keymapActions.replaceContext([UIContext.OnDecision]));
          break;
        default:
          return;
      }
    },
  };
};
