import * as React from "react";
import { empty } from "../../../types/store-state/keymap";
import { mount } from "enzyme";
import { Component } from "./key-handling-container";
import LocatorContext from "../../../locator";

describe("Container", () => {
  describe("Key Handling Container", () => {
    it("render correctly with context", () => {
      // ignore warning
      const locator = {};
      const state = empty();

      const wrapper = mount(
        <LocatorContext.Provider value={locator}>
          <Component keymap={state}>
            <span>text</span>
          </Component>
        </LocatorContext.Provider>
      );

      expect(wrapper.find(Component).find("span")).toHaveLength(1);
    });
  });
});
