import { h } from "preact";
import { render, cleanup } from "@testing-library/preact";

import * as Modal from "./modal";

const Container = ({ role }: { role: string }) => {
  return <div class="foo" role={role} data-testid="container" />;
};

describe("UI kit", () => {
  describe("Modal", () => {
    let root: HTMLElement;
    beforeEach(() => {
      root = document.createElement("div");
      document.body.appendChild(root);
    });

    afterEach(() => {
      cleanup();
    });

    test("render with empty nodes", () => {
      const C = Modal.createComponent({
        container: Container,
      });
      const { getByTestId } = render(<C dialogRoot={root} opened={false} />);

      expect(getByTestId("modal-root")).not.toBeNull();
    });

    test("render with component parameters ", () => {
      const C = Modal.createComponent({
        container: Container,
      });
      const { getByTestId } = render(<C dialogRoot={root} opened={true} container={{ role: "foo" }} />);

      expect(getByTestId("modal-overlay")).not.toBeNull();
    });

    test("render a dialog opened on a dialog root", () => {
      const C = Modal.createComponent({
        container: Container,
      });
      const { getByTestId } = render(<C dialogRoot={root} opened={true} container={{ role: "role" }} />);

      expect(getByTestId("container")).not.toBeNull();
    });
  });
});
