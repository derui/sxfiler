import * as React from "react";
import { empty } from "@/states";
import renderer from "react-test-renderer";
import { Component } from "./main-container";
import LocatorContext from "@/locator";

describe("Container", () => {
  describe("Main Container", () => {
    it("render correctly when context not initialized and empty state", () => {
      // ignore warning
      const locator = {};
      const state = empty();

      const wrapper = renderer
        .create(
          <LocatorContext.Provider value={locator}>
            <Component state={state} />
          </LocatorContext.Provider>
        )
        .toJSON();

      expect(wrapper).toMatchSnapshot();
    });
  });
});
