import * as React from "react";
import * as enzyme from "enzyme";

import * as Manager from "./resize-observer";
import * as ResizeSensor from "./resize-sensor";

afterEach(() => {
  jest.restoreAllMocks();
});

describe("Resize Sensor for React", () => {
  describe("Observer Element", () => {
    it("observe when component mounted", () => {
      const spy = jest.spyOn(Manager, "observe");

      const wrapper = enzyme.mount(
        <ResizeSensor.Component>
          <span>foo</span>
        </ResizeSensor.Component>
      );

      expect(spy.mock.calls).toHaveLength(1);
      expect(wrapper.find("span")).toHaveLength(1);
    });

    it("unobserve when component unmounted", () => {
      const spyObserve = jest.spyOn(Manager, "observe");
      const spyUnobserve = jest.spyOn(Manager, "unobserve");
      const wrapper = enzyme.mount(
        <ResizeSensor.Component>
          <span>foo</span>
        </ResizeSensor.Component>
      );
      wrapper.unmount();

      expect(spyObserve).toHaveBeenCalledTimes(1);
      expect(spyUnobserve).toHaveBeenCalledTimes(1);
    });

    describe("update", () => {
      it("reobserve if handler changed", () => {
        const handler = jest.fn();

        const wrapper = enzyme.mount(
          <ResizeSensor.Component onResize={handler}>
            <span>foo</span>
          </ResizeSensor.Component>
        );

        const observe = jest.spyOn(wrapper.instance() as any, "observe");

        wrapper.setProps({ onResize: () => {} });

        expect(observe).toHaveBeenCalledTimes(1);
      });
    });
  });
});
