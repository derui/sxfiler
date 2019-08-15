import * as React from "react";
import * as enzyme from "enzyme";
import * as ReactDOM from "react-dom";
import { act } from "react-dom/test-utils";

import * as observer from "./observer-manager";

import { Component } from "./observer-root";

let container: HTMLElement | null;

beforeEach(() => {
  container = document.createElement("div");
});

afterEach(() => {
  container = null;
  jest.restoreAllMocks();
});

describe("Intersection Observer for React", () => {
  describe("Observer Root Element", () => {
    it("create root before mount", () => {
      const spy = jest.spyOn(observer, "createRoot");
      const component = enzyme.mount(
        <Component>
          {() => {
            return <span />;
          }}
        </Component>
      );

      expect(spy).toBeCalledTimes(1);
      expect(component.find("span")).toHaveLength(1);
    });

    it("call children with root id", () => {
      let actual;

      const component = enzyme.mount(
        <Component>
          {rootId => {
            actual = rootId;
            return <span />;
          }}
        </Component>
      );

      expect(component.find("span")).toHaveLength(1);
      expect(actual).not.toBeUndefined();
    });

    it("delete root when unmount", () => {
      const spy = spyOn(observer, "deleteRoot");
      act(() => {
        const e = enzyme.mount(
          <Component>
            {() => {
              return <span />;
            }}
          </Component>
        );
        e.unmount();
      });

      expect(spy).toBeCalledTimes(1);
    });
  });
});
