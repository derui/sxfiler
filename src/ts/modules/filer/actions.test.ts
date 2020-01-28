import { actions } from "./actions";
import { ActionTypes } from "./types";
import { Filer, FileWindow } from "@/generated/filer_pb";
import { Side } from "./reducer";

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

      test("create update file window action", () => {
        const fileWindow = new FileWindow();

        expect(actions.updateFileWindow(fileWindow, Side.Left).type).toEqual(ActionTypes.UPDATE_FILE_WINDOW);
      });
    });
  });
});
