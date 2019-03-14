import * as React from "react";
import { mount } from "enzyme";

import { ElementSize, ResizeSensor } from "./index";

afterEach(() => {
  jest.clearAllMocks();
});

describe("Resize Sensor", () => {
  it("call on resize when scroll", done => {
    const callback = (size: ElementSize) => {
      expect(size).toEqual({ height: 10, width: 10 });
      done();
    };

    const wrapper = mount(
      <div className="test">
        <ResizeSensor onResize={callback} getParentSize={() => ({ height: 10, width: 10 })} />
      </div>
    );

    wrapper.simulate("scroll");
  });
});
