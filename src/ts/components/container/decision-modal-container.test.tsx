import { render, findByTestId, fireEvent } from "@testing-library/preact";
import { h } from "preact";

import { Component } from "./decision-modal-container";
import { LocatorContext, Locator } from "@/locator";
import { ModalRootContext } from "@/modal-root";
import { emptyState } from "@/modules";
import * as decision from "@/modules/decision";
import * as commands from "@/commands/internal";
import { FileItem } from "@/generated/filer_pb";

describe("Container", () => {
  describe("Decision modal container", () => {
    let resolveBy = jest.fn();
    let execute = jest.fn();

    function makeMockedLocator(): Locator {
      return {
        commandResolver: {
          register: jest.fn(),
          resolveBy,
          resolveById: jest.fn(),
        },
        commandExecutor: {
          execute,
        },
      };
    }
    afterEach(() => jest.clearAllMocks());

    it("do not render when element is not provided", () => {
      const per = render(
        <LocatorContext.Provider value={makeMockedLocator()}>
          <Component state={emptyState} />
        </LocatorContext.Provider>
      );

      expect(per.queryByTestId("decisionModal")).toBeNull();
    });

    it("render when element is provided", () => {
      const element = document.createElement("div");
      const per = render(
        <LocatorContext.Provider value={makeMockedLocator()}>
          <ModalRootContext.Provider value={{ element }}>
            <Component state={emptyState} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      expect(per.queryByTestId("decisionModal")).toBeDefined();
    });

    it("call command when new name updated", async () => {
      const element = document.createElement("div");
      const locator = makeMockedLocator();
      const state = {
        ...emptyState,
        decision: decision.reducer(decision.emptyState, decision.actions.requireDecisionForCopy("id", new FileItem())),
      };
      const command = jest.fn();
      resolveBy.mockImplementation(() => {
        return command;
      });

      render(
        <LocatorContext.Provider value={locator}>
          <ModalRootContext.Provider value={{ element }}>
            <Component state={state} />
          </ModalRootContext.Provider>
        </LocatorContext.Provider>
      );

      const input = await findByTestId(element, "decisionModal-input");
      fireEvent.input(input, { target: { value: "foo" } });

      expect(resolveBy).toBeCalledWith(commands.descriptors.decisionUpdateNewName);
      expect(execute).toBeCalledWith(command, state, { updatedName: "foo" });
    });
  });
});
