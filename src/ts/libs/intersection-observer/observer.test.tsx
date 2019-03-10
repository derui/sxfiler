import * as React from "react";
import * as enzyme from "enzyme";
import renderer from "react-test-renderer";

import * as observer from "./observer-manager";
import * as Observer from "./observer";

afterEach(() => {
  jest.restoreAllMocks();
});

describe("Intersection Observer for React", () => {
  describe("Observer Element", () => {
    it("throws if component has more than a child", () => {
      const component = (
        <Observer.Component rootId="tmp">
          <span />
          <span />
        </Observer.Component>
      );

      expect(() => renderer.create(component)).toThrowErrorMatchingSnapshot();
    });

    it("observe when component mounted", () => {
      const id = observer.createRoot();
      const instance = observer.registerObserverRoot(id, {
        root: document.createElement("div"),
      });
      const spy = jest.spyOn(instance, "observe");

      const wrapper = enzyme.mount(
        <Observer.Component rootId={id}>
          <span>foo</span>
        </Observer.Component>
      );

      expect(spy.mock.calls).toHaveLength(1);
      expect(wrapper.find("span")).toHaveLength(1);
    });

    it("unobserve when component unmounted", () => {
      const id = observer.createRoot();
      observer.registerObserverRoot(id, {
        root: document.createElement("div"),
      });

      const wrapper = enzyme.mount(
        <Observer.Component rootId={id}>
          <span>foo</span>
        </Observer.Component>
      );
      const spy = jest.spyOn(wrapper.instance() as any, "unobserve");
      wrapper.unmount();

      expect(spy).toHaveBeenCalledTimes(2);
    });

    describe("update", () => {
      it("reobserve if root changed", () => {
        const id = observer.createRoot();
        const id2 = observer.createRoot();
        observer.registerObserverRoot(id, {
          root: document.createElement("div"),
        });
        const handler = jest.fn();

        const wrapper = enzyme.mount(
          <Observer.Component rootId={id} onChange={handler}>
            <span>foo</span>
          </Observer.Component>
        );

        const observe = jest.spyOn(wrapper.instance() as any, "observe");
        const unobserve = jest.spyOn(wrapper.instance() as any, "unobserve");

        wrapper.setProps({ rootId: id2 });

        expect(observe).toHaveBeenCalledTimes(1);
        expect(unobserve).toHaveBeenCalledTimes(1);
      });

      it("reobserve if handler changed", () => {
        const id = observer.createRoot();
        observer.registerObserverRoot(id, {
          root: document.createElement("div"),
        });

        const wrapper = enzyme.mount(
          <Observer.Component rootId={id}>
            <span>foo</span>
          </Observer.Component>
        );

        const observe = jest.spyOn(wrapper.instance() as any, "observe");
        const unobserve = jest.spyOn(wrapper.instance() as any, "unobserve");

        wrapper.setProps({ rootId: id, onChange: () => {} });

        expect(observe).toHaveBeenCalledTimes(1);
        expect(unobserve).toHaveBeenCalledTimes(1);
      });
    });
  });
});
