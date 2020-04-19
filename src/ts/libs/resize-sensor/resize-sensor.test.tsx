import { h } from "preact";
import { render, cleanup } from "@testing-library/preact";
import * as Manager from "./resize-observer";
import * as ResizeSensor from "./resize-sensor";

afterEach(() => {
  jest.restoreAllMocks();
});

afterEach(cleanup);

describe("Resize Sensor for preact", () => {
  describe("Observer Element", () => {
    it("observe when component mounted", () => {
      const spy = jest.spyOn(Manager, "observe");

      const wrapper = render(
        <ResizeSensor.Component>
          {(ref) => (
            <span date-testid="span" ref={ref}>
              foo
            </span>
          )}
        </ResizeSensor.Component>
      );

      expect(spy.mock.calls).toHaveLength(1);
      expect(wrapper.queryByTestId("span")).toBeDefined();
    });

    it("unobserve when component unmounted", () => {
      const spyObserve = jest.spyOn(Manager, "observe");
      const spyUnobserve = jest.spyOn(Manager, "unobserve");
      const wrapper = render(
        <ResizeSensor.Component>
          {(ref) => (
            <span date-testid="test" ref={ref}>
              foo
            </span>
          )}
        </ResizeSensor.Component>
      );
      wrapper.unmount();

      expect(spyObserve).toHaveBeenCalledTimes(1);
      expect(spyUnobserve).toHaveBeenCalledTimes(1);
    });

    describe("update", () => {
      it("reobserve if handler changed", () => {
        const handler = jest.fn();

        const wrapper = render(
          <ResizeSensor.Component onResize={handler}>
            {(ref) => (
              <span data-testid="test" ref={ref}>
                foo
              </span>
            )}
          </ResizeSensor.Component>
        );

        const observe = jest.spyOn(Manager, "observe");
        wrapper.unmount();

        wrapper.rerender(
          <ResizeSensor.Component onResize={() => {}}>
            {(ref) => (
              <span data-testid="test" ref={ref}>
                foo
              </span>
            )}
          </ResizeSensor.Component>
        );

        expect(observe).toHaveBeenCalledTimes(1);
      });
    });
  });
});
