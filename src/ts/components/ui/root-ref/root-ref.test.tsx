import * as React from "react";
import { mount } from "enzyme";

import * as Element from "../element/element";
import { Component } from "./root-ref";
import { ForwardedRef } from "../util";

describe("UI Kit", () => {
  describe("RootRef", () => {
    it("call reference if mounted", () => {
      const ref = React.createRef<HTMLElement>();
      mount(
        <Component rootRef={ref}>
          <Element.Component>foo</Element.Component>
        </Component>
      );

      expect(ref.current).not.toBeUndefined();
      expect(ref.current!.textContent).toEqual("foo");
    });

    it("allow to pass function to rootRef", () => {
      const ref = jest.fn();
      mount(
        <Component rootRef={ref}>
          <Element.Component>foo</Element.Component>
        </Component>
      );

      expect(ref).toBeCalledTimes(1);
    });

    it("allow to get ref nested component", () => {
      const ref = jest.fn();
      const Comp: Element.ComponentType = props => (
        <Element.Component forwardedRef={props.forwardedRef}>far</Element.Component>
      );

      mount(
        <Component rootRef={ref}>
          <Comp />
        </Component>
      );

      expect(ref).toBeCalledTimes(1);
    });

    it("allow to get ref with ForwarededRef type", () => {
      const ref = jest.fn();
      const Comp: React.FC<ForwardedRef> = props => (
        <Element.Component forwardedRef={props.forwardedRef}>far</Element.Component>
      );

      mount(
        <Component rootRef={ref}>
          <Comp />
        </Component>
      );

      expect(ref).toBeCalledTimes(1);
    });
  });
});
