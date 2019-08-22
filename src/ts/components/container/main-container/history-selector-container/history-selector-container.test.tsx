import * as React from "react";
import { mount } from "enzyme";

import { Component } from "./history-selector-container";
import { Theme, ThemeProvider } from "@/components/theme";
import { Component as Completer } from "@/components/project/completer";
import * as H from "@/states/history";
import { ModalRootContext } from "@/modal-root";
import { LocatorContext, Locator } from "@/locator";
import { createUseCase } from "@/usecases/history/read";
import { Side } from "@/states/file-list";

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
      const state = H.empty();
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
      const state = H.open(Side.Left)(H.empty());
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
      const state = H.open(Side.Left)(H.empty());
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
