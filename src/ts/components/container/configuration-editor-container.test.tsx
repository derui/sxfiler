import { h } from "preact";
import { render as renderToString } from "preact-render-to-string";

import { emptyState } from "@/modules";
import { Component } from "./configuration-editor-container";
import { Locator } from "@/locator";
import { cleanup, render, fireEvent, act } from "@testing-library/preact";
import { Configurations } from "@/configurations";

describe("Container", () => {
  describe("Configuration Editor Container", () => {
    const executor = jest.fn();
    const resolveBy = jest.fn();
    let locator: Locator | null;

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
    });

    afterEach(() => {
      locator = null;
      jest.clearAllMocks();
      cleanup();
    });

    it("render correctly when context not initialized and empty state", () => {
      const locator = {};
      const state = emptyState;

      const wrapper = renderToString(<Component state={state} locator={locator} />);

      expect(wrapper).toMatchSnapshot();
    });

    it("call command if section clicked", async () => {
      const state = emptyState;

      const query = render(<Component state={state} locator={locator!!} />);

      const section = await query.findAllByTestId("configuration-navigator-section");
      const command = jest.fn();
      resolveBy.mockImplementation(() => command);

      act(() => {
        fireEvent.click(section[0]);
      });

      expect(executor).toBeCalledWith(command, state, { section: Configurations.definitions.general.sections[0] });
    });
  });
});
