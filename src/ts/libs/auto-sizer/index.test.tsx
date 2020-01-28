import { h } from "preact";
import { render as mount } from "@testing-library/preact";
import { render } from "preact-render-to-string";

import { AutoSizer } from "./index";

describe("Auto Sizer", () => {
  it("do not render children when first mounted", () => {
    const wrapper = mount(
      <AutoSizer>
        {() => {
          return <span data-testid="test">foo</span>;
        }}
      </AutoSizer>
    );

    expect(wrapper.queryByTestId("test")).toBeNull();
  });

  it("can set container element", () => {
    const tree = render(
      <AutoSizer container="section">
        {() => {
          return <span>foo</span>;
        }}
      </AutoSizer>
    );

    expect(tree).toMatchSnapshot();
  });
});
