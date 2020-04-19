import { h } from "preact";
import { render, fireEvent, cleanup, getByTestId, findByTestId } from "@testing-library/preact";

import { Component } from "./completer-container";
import * as C from "@/modules/completer";
import * as M from "@/modules";
import { ModalRootContext } from "@/modal-root";
import { LocatorContext, Locator } from "@/locator";

describe("Container", () => {
  const executor = jest.fn();
  const resolveBy = jest.fn();
  let locator: Locator | null;
  let modal: HTMLElement;

  function makeMockedLocator(): Locator {
    return {
      commandExecutor: {
        execute: executor,
      },
      commandResolver: {
        register: jest.fn(),
        resolveById: jest.fn(),
        resolveBy: resolveBy,
      },
    };
  }
  beforeEach(() => {
    locator = makeMockedLocator();
    modal = document.createElement("div");
  });

  afterEach(() => {
    locator = null;
    jest.clearAllMocks();
    cleanup();
  });

  describe("History Selector", () => {
    it("should not open completer when empty", () => {
      const state = M.emptyState;
      const wrapper = render(
        <LocatorContext.Provider value={locator!!}>
          <ModalRootContext.Provider value={{ element: modal }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      expect(wrapper.queryByTestId("completer-root")).toBeDefined();
    });

    it("should print title", () => {
      const state = { ...M.emptyState, completer: C.reducer(C.emptyState, C.actions.open("History")) };
      const wrapper = render(
        <LocatorContext.Provider value={locator!!}>
          <ModalRootContext.Provider value={{ element: modal }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      expect(wrapper.queryByText("History")).toBeDefined();
    });

    it("should call use case with inputted text", async () => {
      const state = { ...M.emptyState, completer: C.reducer(C.emptyState, C.actions.open("title")) };
      render(
        <LocatorContext.Provider value={locator!!}>
          <ModalRootContext.Provider value={{ element: modal }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      const command = jest.fn();
      resolveBy.mockImplementation(() => command);

      const input = await findByTestId(modal, "completer-input");
      fireEvent.input(input, { target: { value: "foo" } });

      expect(executor).toBeCalledWith(command, state, { input: "foo" });
    });
  });
});
