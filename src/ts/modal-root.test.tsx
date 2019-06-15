import * as React from "react";
import { mount } from "enzyme";
import ModalRootContext from "./modal-root";

describe("Contexts", () => {
  describe("ModalRoot", () => {
    it("get undefined when default provider", () => {
      const wrapper = mount(
        <ModalRootContext.Provider value={{ element: null }}>
          <ModalRootContext.Consumer>
            {context => {
              if (context.element) {
                return <span>give element</span>;
              }
              return null;
            }}
          </ModalRootContext.Consumer>
        </ModalRootContext.Provider>
      );

      expect(wrapper.find("span")).toHaveLength(0);
    });

    it("get root element", () => {
      const element = document.createElement("div");
      const wrapper = mount(
        <ModalRootContext.Provider value={{ element }}>
          <ModalRootContext.Consumer>
            {context => {
              if (context.element) {
                return <span>give element</span>;
              }
              return null;
            }}
          </ModalRootContext.Consumer>
        </ModalRootContext.Provider>
      );

      expect(wrapper.find("span")).toHaveLength(1);
    });
  });
});
