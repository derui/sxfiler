import { emptyState, reducer, DecisionRequiredOp, DecisionAction } from "./reducer";
import { actions } from "./actions";
import { pipe } from "@/libs/fn";
import { FileItem } from "@/generated/filer_pb";

describe("Modules", () => {
  describe("Decision", () => {
    describe("Reducer", () => {
      test("setup for decision when decision required from copy operation", () => {
        let item = new FileItem();
        let state = reducer(undefined, actions.requireDecisionForCopy("id", item));

        expect(state.currentOp).toEqual(DecisionRequiredOp.Copy);
        expect(state.targetItem).toEqual(item.toObject());
        expect(state.processId).toEqual("id");
        expect(state.processing).toBeTruthy();
        expect(state.selectingActionIndex).toEqual(0);
        expect(state.selectableActions).toEqual(
          expect.arrayContaining([{ kind: DecisionAction.Overwrite }, { kind: DecisionAction.Rename, newName: "" }])
        );
      });

      test("setup for decision when decision required from move operation", () => {
        let item = new FileItem();
        item.setName("initial.txt");
        let state = reducer(undefined, actions.requireDecisionForMove("id", item));

        expect(state.currentOp).toEqual(DecisionRequiredOp.Move);
        expect(state.targetItem).toEqual(item.toObject());
        expect(state.processId).toEqual("id");
        expect(state.processing).toBeTruthy();
        expect(state.selectingActionIndex).toEqual(0);
        expect(state.selectableActions).toEqual(
          expect.arrayContaining([
            { kind: DecisionAction.Overwrite },
            { kind: DecisionAction.Rename, newName: "initial.txt" },
          ])
        );
      });

      test("setup for decision when decision required from delete operation", () => {
        let item = new FileItem();
        let state = reducer(undefined, actions.requireDecisionForDelete("id", item));

        expect(state.currentOp).toEqual(DecisionRequiredOp.Delete);
        expect(state.targetItem).toEqual(item.toObject());
        expect(state.processId).toEqual("id");
        expect(state.processing).toBeTruthy();
        expect(state.selectingActionIndex).toEqual(0);
        expect(state.selectableActions).toEqual([{ kind: DecisionAction.Confirm }]);
      });

      test("finish current processing", () => {
        let item = new FileItem();
        let state = pipe(
          (v) => reducer(v, actions.requireDecisionForCopy("id", item)),
          (v) => reducer(v, actions.finish())
        )(undefined);

        expect(state.currentOp).not.toBeUndefined();
        expect(state.targetItem).not.toBeUndefined();
        expect(state.processing).toBeFalsy();
        expect(state.processId).not.toBeUndefined();
      });

      test("reset processing informations", () => {
        let item = new FileItem();
        let state = pipe(
          (v) => reducer(v, actions.requireDecisionForCopy("id", item)),
          (v) => reducer(v, actions.finish()),
          (v) => reducer(v, actions.reset())
        )(undefined);

        expect(state).toEqual(emptyState);
      });

      test("select next action", () => {
        let item = new FileItem();
        let state = pipe(
          (v) => reducer(v, actions.requireDecisionForCopy("id", item)),
          (v) => reducer(v, actions.selectNextAction())
        )(undefined);

        expect(state.selectingActionIndex).toEqual(1);
      });

      test("do not move forward when index reached the end of selectable actions", () => {
        let item = new FileItem();
        let state = pipe(
          (v) => reducer(v, actions.requireDecisionForCopy("id", item)),
          (v) => reducer(v, actions.selectNextAction()),
          (v) => reducer(v, actions.selectNextAction()),
          (v) => reducer(v, actions.selectNextAction())
        )(undefined);

        expect(state.selectingActionIndex).toEqual(1);
      });

      test("select previous action", () => {
        let item = new FileItem();
        let state = pipe(
          (v) => reducer(v, actions.requireDecisionForCopy("id", item)),
          (v) => reducer(v, actions.selectNextAction()),
          (v) => reducer(v, actions.selectPreviousAction())
        )(undefined);

        expect(state.selectingActionIndex).toEqual(0);
      });

      test("do not move backword when index reached the start of selectable actions", () => {
        let item = new FileItem();
        let state = pipe(
          (v) => reducer(v, actions.requireDecisionForCopy("id", item)),
          (v) => reducer(v, actions.selectNextAction()),
          (v) => reducer(v, actions.selectPreviousAction()),
          (v) => reducer(v, actions.selectPreviousAction())
        )(undefined);

        expect(state.selectingActionIndex).toEqual(0);
      });
    });
  });
});
