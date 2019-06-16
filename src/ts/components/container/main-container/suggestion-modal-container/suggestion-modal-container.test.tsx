import { mount } from "enzyme";
import * as React from "react";

import * as State from "../../../../states/task-interaction";
import { Component } from "./suggestion-modal-container";
import { createSuggestions, createSuggestion, SuggestionKind } from "../../../../domains/task-suggestion";
import LocatorContext, { Locator } from "../../../../locator";
import ModalRootContext from "../../../../modal-root";
import * as SuggestionModal from "../../../project/suggestion-modal/suggestion-modal";
import { createOverwritePayload } from "../../../../domains/task-reply";

describe("Container", () => {
  describe("Suggestion modal container", () => {
    let state = State.empty();
    state = State.giveSuggestions(
      state,
      createSuggestions({
        taskId: "task",
        nodeName: "node",
        suggestions: [createSuggestion({ kind: SuggestionKind.Overwrite })],
      })
    );

    const executor = { execute: jest.fn() };

    function makeMockedLocator(): Locator {
      return {
        context: {
          use: () => executor,
        },
      };
    }
    afterEach(() => jest.clearAllMocks());

    it("do not render when element is not provided", () => {
      const wrapper = mount(
        <LocatorContext.Provider value={makeMockedLocator()}>
          <Component state={state} />
        </LocatorContext.Provider>
      );

      expect(wrapper.isEmptyRender()).toBeTruthy();
    });

    it("render when element is provided", () => {
      const element = document.createElement("div");
      const wrapper = mount(
        <LocatorContext.Provider value={makeMockedLocator()}>
          <ModalRootContext.Provider value={{ element }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      expect(wrapper.find(SuggestionModal.Component)).toHaveLength(1);
    });

    it("open the modal when operating some suggestion", () => {
      const element = document.createElement("div");
      const wrapper = mount(
        <LocatorContext.Provider value={makeMockedLocator()}>
          <ModalRootContext.Provider value={{ element }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      expect(wrapper.find(SuggestionModal.Component).prop("opened")).toBeTruthy();
    });

    it("execute a use case with updated reply", () => {
      const element = document.createElement("div");
      const locator = makeMockedLocator();
      const wrapper = mount(
        <LocatorContext.Provider value={locator}>
          <ModalRootContext.Provider value={{ element }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );
      const payload = createOverwritePayload();

      wrapper
        .find(SuggestionModal.Component)
        .props()
        .container!!.onReply(payload);
      expect(executor.execute).toHaveBeenCalled();
    });
  });
});
