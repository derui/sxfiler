import * as React from "react";
import { mount } from "enzyme";

import { Component } from "./completer-container";
import { Theme, ThemeProvider } from "@/components/theme";
import { Component as Completer } from "@/components/project/completer";
import * as C from "@/states/completer";
import { ModalRootContext } from "@/modal-root";
import { LocatorContext, Locator } from "@/locator";

describe("Container", () => {
  const executor = jest.fn();
  let locator: Locator | null;
  let modal: HTMLElement | null;

  function makeMockedLocator(): Locator {
    return {
      context: {
        use: () => executor,
      },
      client: {
        call: jest.fn(),
        notify: jest.fn(),
      },
    };
  }
  beforeEach(() => {
    locator = makeMockedLocator();
    modal = document.createElement("div");
  });

  afterEach(() => {
    locator = null;
    modal = null;
    jest.clearAllMocks();
  });

  describe("History Selector", () => {
    it("should not open completer when empty", () => {
      const state = C.empty();
      const wrapper = mount(
        <ThemeProvider theme={Theme}>
          <LocatorContext.Provider value={locator!!}>
            <ModalRootContext.Provider value={{ element: modal }}>
              <Component state={state} />
            </ModalRootContext.Provider>
          </LocatorContext.Provider>
        </ThemeProvider>
      );

      expect(wrapper.find(Completer).props().opened).toBeFalsy();
    });

    it("should print title", () => {
      const state = C.open("History")(C.empty());
      const wrapper = mount(
        <ThemeProvider theme={Theme}>
          <LocatorContext.Provider value={locator!!}>
            <ModalRootContext.Provider value={{ element: modal }}>
              <Component state={state} />
            </ModalRootContext.Provider>
          </LocatorContext.Provider>
        </ThemeProvider>
      );

      expect(wrapper.text()).toContain("History");
    });

    it("should call use case with inputted text", () => {
      const state = C.open("title")(C.empty());
      const wrapper = mount(
        <ThemeProvider theme={Theme}>
          <LocatorContext.Provider value={locator!!}>
            <ModalRootContext.Provider value={{ element: modal }}>
              <Component state={state} />
            </ModalRootContext.Provider>
          </LocatorContext.Provider>
        </ThemeProvider>
      );
      wrapper
        .find(Completer)
        .props()
        .onInput("foo");

      expect(executor).toBeCalledWith({ input: "foo" });
    });
  });
});
