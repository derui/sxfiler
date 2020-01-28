import { TypedPublisher, EventCreators } from "./typed-event-hub";
import { Middleware } from "redux";
import { State, Actions } from "./modules";
import * as Decision from "./modules/decision";
import { getCurrentFocusedAction } from "./modules/decision/selectors";

export const create = (eventHub: TypedPublisher): Middleware<{}, State> => {
  return ({ getState }) => (next) => (action: Actions) => {
    const returnValue = next(action);
    const state = getState();

    switch (action.type) {
      case Decision.ActionTypes.FINISH:
        {
          const { currentOp: decisionOp, processId } = state.decision;
          const resultAction = getCurrentFocusedAction(state.decision);
          if (!decisionOp || !processId || !resultAction) {
            break;
          }

          eventHub.publish(
            EventCreators.finishDecision({
              decisionOp,
              processId,
              resultAction,
            })
          );
        }
        break;
      default:
        break;
    }
    return returnValue;
  };
};
