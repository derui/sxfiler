import { h } from "preact";
import { render, cleanup, fireEvent, findByTestId, queryByTestId } from "@testing-library/preact";
import * as Modal from "./index";
import { DecisionAction } from "@/modules/decision/reducer";

describe("Project", () => {
  describe("Decision Modal", () => {
    afterEach(cleanup);

    it("render correctly", () => {
      const root = document.createElement("div");
      render(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            focusedActionKind: DecisionAction.Overwrite,
            selectableActions: [{ kind: DecisionAction.Overwrite }],
            onNewNameChange: () => {},
          }}
        />
      );

      expect(queryByTestId(root, "decisionModal-rename")).toBeDefined();
      expect(queryByTestId(root, "decisionModal-overwrite")).toBeDefined();
    });

    it("render with reply to overwrite", async () => {
      const root = document.createElement("div");
      return new Promise(async (resolve) => {
        render(
          <Modal.Component
            dialogRoot={root}
            opened={true}
            container={{
              focusedActionKind: DecisionAction.Rename,
              selectableActions: [{ kind: DecisionAction.Rename, newName: "empty.js" }],
              onNewNameChange: (newName) => {
                expect(newName).toEqual("newName");
                resolve();
              },
            }}
          />
        );

        const el = await findByTestId(root, "decisionModal-input");
        fireEvent.input(el, { target: { value: "newName" } });
      });
    });
  });
});
