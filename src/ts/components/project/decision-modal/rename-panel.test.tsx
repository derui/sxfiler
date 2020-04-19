import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component } from "./rename-panel";
import { render as mount, fireEvent } from "@testing-library/preact";

describe("Project", () => {
  describe("Rename decision", () => {
    it("render with rename decision", () => {
      const handler = () => {};
      const tree = render(<Component selected={false} onUpdated={handler} itemName={"node"} />);

      expect(tree).toMatchSnapshot();
    });

    it("change style when selected", () => {
      const handler = () => {};
      const tree = render(<Component selected={true} onUpdated={handler} itemName={"node"} />);

      expect(tree).toMatchSnapshot();
    });

    it("do not render with other decision", () => {
      const handler = () => {};
      const tree = render(<Component selected={false} onUpdated={handler} itemName={"node"} />);

      expect(tree).toMatchSnapshot();
    });

    it("call handler when the new name in the component is changed", (done) => {
      const dom = document.createElement("div");
      const handler = (newName: string) => {
        expect(newName).toEqual("foo");
        done();
      };

      const per = mount(<Component selected={true} onUpdated={handler} itemName="bar" />, { baseElement: dom });
      const v = per.getByTestId("decisionModal-input") as HTMLInputElement;
      v.value = "foo";
      fireEvent.input(v, new Event("input"));
    });
  });
});
