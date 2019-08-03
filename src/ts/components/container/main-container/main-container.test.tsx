import * as React from "react";
import renderer from "react-test-renderer";

import { Theme, ThemeProvider } from "@/components/theme";
import { empty } from "@/states";
import { Component } from "./main-container";
import { LocatorContext } from "@/locator";

describe("Container", () => {
  describe("Main Container", () => {
    it("render correctly when context not initialized and empty state", () => {
      // ignore warning
      const locator = {};
      const state = empty();

      const wrapper = renderer
        .create(
          <ThemeProvider theme={Theme}>
            <LocatorContext.Provider value={locator}>
              <Component state={state} />
            </LocatorContext.Provider>
          </ThemeProvider>
        )
        .toJSON();

      expect(wrapper).toMatchSnapshot();
    });
  });
});
