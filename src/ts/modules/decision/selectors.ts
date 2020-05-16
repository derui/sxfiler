import { State, DecisionAction, SelectableAction } from "./reducer";

// get selectable action
export const getCurrentFocusedActionKind = (state: State): DecisionAction | undefined => {
  return state.selectableActions[state.selectingActionIndex]?.kind;
};

export const getCurrentFocusedAction = (state: State): SelectableAction | undefined => {
  return state.selectableActions[state.selectingActionIndex];
};
