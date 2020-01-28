import { h } from "preact";
import { render } from "preact-render-to-string";

import { Component } from "./filer-container";
import { also } from "@/libs/fn";
import { Filer, FileWindow, FileList } from "@/generated/filer_pb";
import { State, Side } from "@/modules/filer/reducer";
import * as N from "@/types/natural-number";

describe("Container", () => {
  let state: State = {
    currentSide: Side.Left,
    currentCursorPosition: {
      left: N.create(0),
      right: N.create(0),
    },
    filer: also(new Filer(), (v) => {
      v.setLeftFileWindow(
        also(new FileWindow(), (v) => {
          v.setFileList(
            also(new FileList(), (v) => {
              v.setLocation("/left");
              v.setItemsList([]);
            })
          );
        })
      );
    }),
  };

  describe("File List Container", () => {
    it("should render correctly", () => {
      const tree = render(<Component state={state} />);

      expect(tree).toMatchSnapshot();
    });
  });
});
