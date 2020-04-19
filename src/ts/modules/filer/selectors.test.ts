import {
  currentFocusingItemSelector,
  getCurrentSideInPb,
  leftSideFileListSelector,
  rightSideFileListSelector,
} from "./selectors";
import { also } from "@/libs/fn";
import { Filer, FileWindow, FileList, FileItem, Side as PbSide } from "@/generated/filer_pb";
import { reducer, Side, emptyState } from "./reducer";
import { actions } from "./actions";

// helper function to create FileItem
const createItem = function createItem(id: string) {
  return also(new FileItem(), (v) => {
    v.setId(id);
  });
};

describe("Modules", () => {
  describe("Filer", () => {
    describe("Selectors", () => {
      test("get current focused item in current side", () => {
        let filer = also(new Filer(), (v) => {
          v.setLeftFileWindow(
            also(new FileWindow(), (v) => {
              v.setFileList(
                also(new FileList(), (v) => {
                  v.setItemsList([createItem("id1"), createItem("id2")]);
                })
              );
            })
          );
        });

        let state = reducer(undefined, actions.update(filer));
        state = reducer(state, actions.cursorDown());

        const selected = currentFocusingItemSelector(state);
        expect(selected?.item?.getId()).toEqual("id2");
        expect(selected?.side).toEqual(PbSide.LEFT);

        state = reducer(state, actions.changeSide());
        expect(state.currentSide).toEqual(Side.Right);
        expect(currentFocusingItemSelector(state)).toBeUndefined();
      });

      test("return undefined if list is empty", () => {
        let filer = also(new Filer(), (v) => {
          v.setLeftFileWindow(
            also(new FileWindow(), (v) => {
              v.setFileList(
                also(new FileList(), (v) => {
                  v.setItemsList([]);
                })
              );
            })
          );
        });

        let state = reducer(undefined, actions.update(filer));
        state = reducer(state, actions.cursorDown());

        const item = currentFocusingItemSelector(state);
        expect(state.currentSide).toEqual(Side.Left);
        expect(item).toBeUndefined();
      });

      test("get current side as generated type", () => {
        let state = reducer(undefined, actions.changeSide());

        let side = getCurrentSideInPb(state);
        expect(side).toEqual(PbSide.RIGHT);
      });

      test("get left file list if it exists", () => {
        expect(leftSideFileListSelector(emptyState)).toBeUndefined();
        const filer = also(new Filer(), (v) => {
          v.setLeftFileWindow(
            also(new FileWindow(), (v) => {
              v.setFileList(
                also(new FileList(), (v) => {
                  v.setItemsList([]);
                })
              );
            })
          );
        });

        const state = reducer(undefined, actions.update(filer));
        expect(leftSideFileListSelector(state)).toEqual(filer.getLeftFileWindow()?.getFileList());
      });

      test("get right file list if it exists", () => {
        expect(rightSideFileListSelector(emptyState)).toBeUndefined();
        const filer = also(new Filer(), (v) => {
          v.setRightFileWindow(
            also(new FileWindow(), (v) => {
              v.setFileList(
                also(new FileList(), (v) => {
                  v.setItemsList([]);
                })
              );
            })
          );
        });

        const state = reducer(undefined, actions.update(filer));
        expect(rightSideFileListSelector(state)).toEqual(filer.getRightFileWindow()?.getFileList());
      });
    });
  });
});
