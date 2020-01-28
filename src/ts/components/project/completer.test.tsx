import { h } from "preact";

import { render, cleanup, fireEvent } from "@testing-library/preact";
import * as Modal from "./completer";
import { getByTestId } from "@testing-library/dom";
import { act } from "preact/test-utils";

describe("Project", () => {
  describe("Completer", () => {
    let root: HTMLElement;
    beforeEach(() => {
      root = document.createElement("div");
      document.body.appendChild(root);
    });

    afterEach(() => {
      cleanup();
    });

    it("render correctly", () => {
      const query = render(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          title="test"
          selectedItemIndex={-1}
          items={[]}
          onInput={() => {}}
        />
      );

      expect(query.getByTestId("completer-container")).not.toBeNull();
      expect(query.getByTestId("completer-input")).not.toBeNull();
    });

    it("render with a candidate", () => {
      const query = render(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          title="test"
          selectedItemIndex={-1}
          items={[{ id: "id", after: "value", before: "", matched: "" }]}
          onInput={() => {}}
        />
      );

      expect(query.getAllByTestId("completer-candidateItem")).toHaveLength(1);
    });

    it("render with candidates", () => {
      const query = render(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          title="test"
          selectedItemIndex={-1}
          items={[
            { id: "id", after: "value", before: "", matched: "" },
            { id: "id2", after: "value2", before: "", matched: "" },
          ]}
          onInput={() => {}}
        />
      );

      expect(query.getAllByTestId("completer-candidateItem")).toHaveLength(2);
    });

    it("call callback when change input", async () => {
      const root = document.createElement("div");
      render(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          title="test"
          selectedItemIndex={-1}
          items={[
            { id: "id", before: "", matched: "", after: "value" },
            { id: "id2", before: "", matched: "", after: "value2" },
          ]}
          onInput={(v) => {
            expect(v).toEqual("foo");
          }}
        />
      );

      const input = getByTestId(root, "completer-input") as HTMLInputElement;

      act(() => {
        input.value = "foo";
      });
      fireEvent.input(input, new window.Event("input", { bubbles: true }));
    });
  });
});
