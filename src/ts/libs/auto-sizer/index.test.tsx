import * as React from "react";
import { mount } from "enzyme";
import renderer from "react-test-renderer";

import AutoSizer from "./index";

describe("Auto Sizer", () => {
  it("do not render children when first mounted", () => {
    const wrapper = mount(
      <AutoSizer>
        {() => {
          return <span>foo</span>;
        }}
      </AutoSizer>
    );

    expect(wrapper.find("span").length).toEqual(0);
  });

  it("can set container element", () => {
    const tree = renderer
      .create(
        <AutoSizer container="section">
          {() => {
            return <span>foo</span>;
          }}
        </AutoSizer>
      )
      .toJSON();

    expect(tree).toMatchSnapshot();
  });
});
