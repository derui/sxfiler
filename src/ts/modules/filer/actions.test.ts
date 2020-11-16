import { actions } from "./actions";
import { ActionTypes } from "./types";
import { FileList, Filer } from "@/generated/filer_pb";

describe("Modules", () => {
  describe("Filer", () => {
    describe("Actions", () => {
      test("create cursor up action", () => {
        const action = actions.cursorUp();
        expect(action.type).toEqual(ActionTypes.CURSOR_UP);
      });

      test("create cursor down action", () => {
        const action = actions.cursorDown();
        expect(action.type).toEqual(ActionTypes.CURSOR_DOWN);
      });

      test("create update action", () => {
        const filer = new Filer();

        expect(actions.update(filer).type).toEqual(ActionTypes.UPDATE);
      });

      test("create change side action", () => {
        const action = actions.changeSide();
        expect(action.type).toEqual(ActionTypes.CHANGE_SIDE);
      });

      test("create apply file events action", () => {
        expect(actions.applyEvents("id", []).type).toEqual(ActionTypes.APPLY_EVENTS);
      });

      test("create apply file list event action", () => {
        expect(actions.applyFileListEvent(0, new FileList()).type).toEqual(ActionTypes.APPLY_FILE_LIST_EVENT);
      });

      test("create action to focus item", () => {
        const action = actions.focusItem("id");
        expect(action.type).toEqual(ActionTypes.FOCUS_ITEM);
        expect(action.payload).toEqual({ itemId: "id" });
      });
    });
  });
});
