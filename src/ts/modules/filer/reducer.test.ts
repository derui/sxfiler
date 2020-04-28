import { reducer, Side } from "./reducer";
import { actions } from "./actions";
import { pipe, also } from "@/libs/fn";
import { Filer, FileWindow, FileList, FileItem } from "@/generated/filer_pb";

describe("Modules", () => {
  describe("Filer", () => {
    describe("Reducer", () => {
      const filer = also(new Filer(), (v) => {
        v.setLeftFileWindow(
          also(new FileWindow(), (v) => {
            v.setFileList(
              also(new FileList(), (v) => {
                v.setLocation("location");
                v.setItemsList([
                  also(new FileItem(), (v) => {
                    v.setId("id1");
                    v.setName("name1");
                  }),
                  also(new FileItem(), (v) => {
                    v.setId("id2");
                    v.setName("name2");
                  }),
                  also(new FileItem(), (v) => {
                    v.setId("id3");
                    v.setName("name3");
                  }),
                ]);
              })
            );
          })
        );
      });

      test("do not move cursor when the given filer has same location", () => {
        const state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.update(filer))
        )(undefined);

        expect(state.currentCursorPosition.left.value).toEqual(1);
      });

      test("reset cursor position when the given filer has difference location", () => {
        const movedFiler = also(filer.clone(), (v) => {
          v.setLeftFileWindow(
            also(v.getLeftFileWindow()!!.clone(), (v) => {
              v.setFileList(
                also(v.getFileList()!!.clone(), (v) => {
                  v.setLocation("difference");
                })
              );
            })
          );
        });
        const state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.update(movedFiler))
        )(undefined);

        expect(state.currentCursorPosition.left.value).toEqual(0);
      });

      test("move current cursor position to down when cursor down action given", () => {
        const state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown())
        )(undefined);

        expect(state.currentCursorPosition.left.value).toEqual(1);
      });

      test("move current cursor position to up when cursor up action given", () => {
        const state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.cursorUp())
        )(undefined);

        expect(state.currentCursorPosition.left.value).toEqual(1);
      });

      test("do not move cursor if it reach top of list", () => {
        const state = reducer(undefined, actions.cursorUp());

        expect(state.currentCursorPosition.left.value).toEqual(0);
      });

      test("change side", () => {
        let state = reducer(undefined, actions.changeSide());

        expect(state.currentSide).toEqual(Side.Right);

        state = reducer(state, actions.changeSide());

        expect(state.currentSide).toEqual(Side.Left);
      });

      test("update file window specified side", () => {
        let state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.cursorDown())
        )(undefined);

        const fileWindow = new FileWindow();
        state = reducer(state, actions.updateFileWindow(fileWindow, Side.Left));

        expect(state.filer?.getLeftFileWindow()?.toObject()).toEqual(fileWindow.toObject());
        expect(state.currentCursorPosition.left.value).toEqual(0);
      });

      test("update file window and cursor position", () => {
        let state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.cursorDown())
        )(undefined);

        const fileWindow = filer.getLeftFileWindow()!!;
        state = reducer(state, actions.updateFileWindow(fileWindow, Side.Left));

        expect(state.filer?.getLeftFileWindow()?.toObject()).toEqual(fileWindow.toObject());
        expect(state.currentCursorPosition.left.value).toEqual(2);
      });

      test("revise cursor position when number of items less than before updating", () => {
        const updatedFiler = also(new Filer(), (v) => {
          v.setLeftFileWindow(
            also(new FileWindow(), (v) => {
              v.setFileList(
                also(new FileList(), (v) => {
                  v.setLocation("location");
                  v.setItemsList([
                    also(new FileItem(), (v) => {
                      v.setId("id1");
                      v.setName("name1");
                    }),
                    also(new FileItem(), (v) => {
                      v.setId("id2");
                      v.setName("name2");
                    }),
                  ]);
                })
              );
            })
          );
        });

        let state = pipe(
          (v) => reducer(v, actions.update(filer)),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.update(updatedFiler))
        )(undefined);

        const fileWindow = updatedFiler.getLeftFileWindow()!!;
        state = reducer(state, actions.updateFileWindow(fileWindow, Side.Left));

        expect(state.filer?.getLeftFileWindow()?.toObject()).toEqual(fileWindow.toObject());
        expect(state.currentCursorPosition.left.value).toEqual(1);
      });

      test("focus specified item of the current side", () => {
        let state = pipe((v) => reducer(v, actions.update(filer)))(undefined);

        const fileWindow = filer.getLeftFileWindow()!!;
        state = reducer(state, actions.focusItem("id3"));

        expect(state.filer?.getLeftFileWindow()?.toObject()).toEqual(fileWindow.toObject());
        expect(state.currentCursorPosition.left.value).toEqual(2);
      });
    });
  });
});
