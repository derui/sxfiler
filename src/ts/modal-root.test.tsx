import { h } from "preact";
import { render, cleanup } from "@testing-library/preact";
import { ModalRootContext } from "./modal-root";

describe("Contexts", () => {
  describe("ModalRoot", () => {
    afterEach(cleanup);

    it("get undefined when default provider", () => {
      const wrapper = render(
        <ModalRootContext.Provider value={{ element: null }}>
          <ModalRootContext.Consumer>
            {(context) => {
              if (context.element) {
                return <span data-testid="test">give element</span>;
              }
              return null;
            }}
          </ModalRootContext.Consumer>
        </ModalRootContext.Provider>
      );

      expect(wrapper.queryByTestId("test")).toBeNull();
    });

    it("get root element", () => {
      const element = document.createElement("div");
      const wrapper = render(
        <ModalRootContext.Provider value={{ element }}>
          <ModalRootContext.Consumer>
            {(context) => {
              if (context.element) {
                return <span data-testid="test">give element</span>;
              }
              return null;
            }}
          </ModalRootContext.Consumer>
        </ModalRootContext.Provider>
      );

      expect(wrapper.getByTestId("test")).toBeDefined();
    });
  });
});
