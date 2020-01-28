import * as S from "./selectors";
import { State, emptyState, DecisionAction } from "./reducer";

describe("Modules", () => {
  describe("Decision", () => {
    describe("Selectors", () => {
      test("get current action kind that is selected", () => {
        const action = {
          kind: "rename",
          newName: "newName",
        };
        const state = {
          selectingActionIndex: 1,
          selectableActions: [{ kind: DecisionAction.Overwrite }, action],
        } as State;

        expect(S.getCurrentFocusedActionKind(state)).toEqual(DecisionAction.Rename);
      });

      test("return undefined as kind when can not selectable any actions", () => {
        const state = emptyState;

        expect(S.getCurrentFocusedActionKind(state)).toBeUndefined();
      });

      test("get current action selected", () => {
        const action = {
          kind: "rename",
          newName: "newName",
        };
        const state = {
          selectingActionIndex: 1,
          selectableActions: [{ kind: DecisionAction.Overwrite }, action],
        } as State;

        expect(S.getCurrentFocusedAction(state)).toEqual(action);
      });

      test("return undefined when can not selectable any actions", () => {
        const state = emptyState;

        expect(S.getCurrentFocusedAction(state)).toBeUndefined();
      });
    });
  });
});
