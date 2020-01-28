import { actions } from "./actions";
import { ActionTypes } from "./types";
import { FileItem } from "@/generated/filer_pb";

describe("Modules", () => {
  describe("Decision", () => {
    describe("Actions", () => {
      test("create action to require decision for copy", () => {
        let fileItem = new FileItem();
        let action = actions.requireDecisionForCopy("id", fileItem);

        expect(action.type).toEqual(ActionTypes.REQUIRE_DECISION_FOR_COPY);
        expect(action.payload.item).toBe(fileItem);
        expect(action.payload.processId).toEqual("id");
      });

      test("create action to require decision for delete", () => {
        let fileItem = new FileItem();
        let action = actions.requireDecisionForDelete("id", fileItem);

        expect(action.type).toEqual(ActionTypes.REQUIRE_DECISION_FOR_DELETE);
        expect(action.payload.item).toBe(fileItem);
        expect(action.payload.processId).toEqual("id");
      });

      test("create action to require decision for move", () => {
        let fileItem = new FileItem();
        let action = actions.requireDecisionForMove("id", fileItem);

        expect(action.type).toEqual(ActionTypes.REQUIRE_DECISION_FOR_MOVE);
        expect(action.payload.item).toBe(fileItem);
        expect(action.payload.processId).toEqual("id");
      });

      test("create action to finish current process", () => {
        let action = actions.finish();

        expect(action.type).toEqual(ActionTypes.FINISH);
      });

      test("create action to select next action", () => {
        const action = actions.selectNextAction();
        expect(action.type).toEqual(ActionTypes.SELECT_NEXT_ACTION);
      });

      test("create action to select previous action", () => {
        const action = actions.selectPreviousAction();
        expect(action.type).toEqual(ActionTypes.SELECT_PREVIOUS_ACTION);
      });

      test("create action to update new name", () => {
        const action = actions.updateNewName("newName");
        expect(action.type).toEqual(ActionTypes.UPDATE_NEW_NAME);
        expect(action.payload).toEqual({ newName: "newName" });
      });

      test("create action to reset processing", () => {
        const action = actions.reset();
        expect(action.type).toEqual(ActionTypes.RESET);
        expect(action.payload).toEqual({});
      });
    });
  });
});
