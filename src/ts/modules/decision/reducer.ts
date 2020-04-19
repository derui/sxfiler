import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { FileItem } from "@/generated/filer_pb";
import { ObjectEnum } from "@/utils";

// Operation type to be required from server.
export const DecisionRequiredOp = {
  Copy: "Copy",
  Move: "Move",
  Delete: "Delete",
} as const;
export type DecisionRequiredOp = ObjectEnum<typeof DecisionRequiredOp>;

// types of actions to decision
export const DecisionAction = {
  Overwrite: "overwrite",
  Rename: "rename",
  Confirm: "confirm",
} as const;
export type DecisionAction = ObjectEnum<typeof DecisionAction>;

export type OverwriteAction = {
  kind: "overwrite";
};

export type ConfirmAction = {
  kind: "confirm";
};

export type RenameAction = {
  kind: "rename";
  newName: string;
};

export type SelectableAction = OverwriteAction | RenameAction | ConfirmAction;

// state of type. Please redefine to what you want.
export type State = Readonly<{
  currentOp?: DecisionRequiredOp;
  targetItem?: FileItem.AsObject;
  processId?: string;
  processing: boolean;
  selectingActionIndex: number;
  selectableActions: SelectableAction[];
}>;

/**
 * empty state of `State`
 */
export const emptyState: State = Object.freeze({
  processing: false,
  selectingActionIndex: 0,
  selectableActions: [],
});

/**
 * reducer for `ActionTypes.FINISH`
 */
const finish = (state: State): State => {
  return Object.freeze({ ...state, processing: false });
};

/**
 * reducer for `ActionTypes.RESET`
 */
const reset = () => emptyState;

/**
 * common reducer for `ActionTypes.REQUIRE_DECISION_FOR_*`
 */
const requireDecisionFor = (
  state: State,
  payload: { item: FileItem; processId: string },
  op: DecisionRequiredOp
): State => {
  let selectableActions: SelectableAction[] = [
    { kind: "overwrite" },
    { kind: "rename", newName: payload.item.getName() },
  ];
  if (op === DecisionRequiredOp.Delete) {
    selectableActions = [{ kind: "confirm" }];
  }

  return Object.freeze({
    ...state,
    processing: true,
    targetItem: payload.item.toObject(),
    currentOp: op,
    processId: payload.processId,
    selectingActionIndex: 0,
    selectableActions,
  });
};

const selectNextAction = (state: State): State => {
  return Object.freeze({
    ...state,
    selectingActionIndex: Math.min(state.selectingActionIndex + 1, state.selectableActions.length - 1),
  });
};

const selectPreviousAction = (state: State): State => {
  return Object.freeze({
    ...state,
    selectingActionIndex: Math.max(state.selectingActionIndex - 1, 0),
  });
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.SELECT_NEXT_ACTION:
      return selectNextAction(state);
    case ActionTypes.SELECT_PREVIOUS_ACTION:
      return selectPreviousAction(state);
    case ActionTypes.FINISH:
      return finish(state);
    case ActionTypes.RESET:
      return reset();
    case ActionTypes.REQUIRE_DECISION_FOR_COPY:
      return requireDecisionFor(state, action.payload, DecisionRequiredOp.Copy);
    case ActionTypes.REQUIRE_DECISION_FOR_MOVE:
      return requireDecisionFor(state, action.payload, DecisionRequiredOp.Move);
    case ActionTypes.REQUIRE_DECISION_FOR_DELETE:
      return requireDecisionFor(state, action.payload, DecisionRequiredOp.Delete);
    default:
      return state;
  }
};
