import { actions } from "./actions";
import { ActionTypes } from "./types";
import { Candidate } from "@/generated/completer_pb";

describe("Modules", () => {
  describe("Completer", () => {
    describe("Actions", () => {
      test("create action to cursor down", () => {
        const action = actions.cursorDown();
        expect(action.type).toEqual(ActionTypes.CURSOR_DOWN);
      });

      test("create action to cursor up", () => {
        const action = actions.cursorUp();
        expect(action.type).toEqual(ActionTypes.CURSOR_UP);
      });

      test("create action to update candidates", () => {
        const candidates = [new Candidate()];
        const action = actions.updateCandidates(candidates);

        expect(action.type).toEqual(ActionTypes.UPDATE_CANDIDATES);
        expect(action.payload.candidates).toEqual(candidates);
      });

      test("create action to open", () => {
        const action = actions.open("title");

        expect(action.type).toEqual(ActionTypes.OPEN);
        expect(action.payload.title).toEqual("title");
      });

      test("create action to close", () => {
        const action = actions.close();

        expect(action.type).toEqual(ActionTypes.CLOSE);
      });
    });
  });
});
