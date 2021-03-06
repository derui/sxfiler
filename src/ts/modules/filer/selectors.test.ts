import {
  currentFocusingItemSelector,
  getCurrentSideInPb,
  leftSideFileListSelector,
  rightSideFileListSelector,
} from "./selectors";
import { also } from "@/libs/fn";
import { Filer, FileWindow, FileList, FileItem, Side as PbSide, FileItemOrder } from "@/generated/filer_pb";
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
        const fileItems = [
          also(new FileItem(), (v) => {
            v.setId("1");
          }),
          also(new FileItem(), (v) => {
            v.setId("2");
          }),
        ];

        const filer = also(new Filer(), (v) => {
          v.setLeftFileWindow(
            also(new FileWindow(), (v) => {
              v.setFileList(
                also(new FileList(), (v) => {
                  v.setFileItemOrdersList([
                    also(new FileItemOrder(), (v) => {
                      v.setFileId("1");
                      v.setSortLevel(2);
                    }),
                    also(new FileItemOrder(), (v) => {
                      v.setFileId("2");
                      v.setSortLevel(1);
                    }),
                  ]);
                  v.setItemsList(fileItems);
                })
              );
            })
          );
        });

        const expected = filer.getLeftFileWindow()?.getFileList()?.clone();
        expected?.setItemsList([fileItems[1], fileItems[0]]);

        const state = reducer(undefined, actions.update(filer));
        expect(leftSideFileListSelector(state)?.toObject()).toEqual(expected?.toObject());
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
        expect(rightSideFileListSelector(state)?.toObject()).toEqual(
          filer.getRightFileWindow()?.getFileList()?.toObject()
        );
      });
    });
  });
});
