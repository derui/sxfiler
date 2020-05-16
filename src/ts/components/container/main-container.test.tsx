import { h } from "preact";
import { render } from "preact-render-to-string";

import { emptyState } from "@/modules";
import { Component } from "./main-container";
import { LocatorContext } from "@/locator";

describe("Container", () => {
  describe("Main Container", () => {
    it("render correctly when context not initialized and empty state", () => {
      const locator = {};
      const state = emptyState;

      const wrapper = render(
        <LocatorContext.Provider value={locator}>
          <Component state={state} />
        </LocatorContext.Provider>
      );

      expect(wrapper).toMatchSnapshot();
    });
  });
});
