import { h } from "preact";
import { render } from "preact-render-to-string";

import { State } from "@/modules";
import { Component } from "./main-container";
import { LocatorContext } from "@/locator";
import { emptyState } from "@/modules/filer";

describe("Container", () => {
  describe("Main Container", () => {
    it("render correctly when context not initialized and empty state", () => {
      const locator = {};
      const state = {
        filer: emptyState,
      } as State;

      const wrapper = render(
        <LocatorContext.Provider value={locator}>
          <Component state={state} />
        </LocatorContext.Provider>
      );

      expect(wrapper).toMatchSnapshot();
    });
  });
});
