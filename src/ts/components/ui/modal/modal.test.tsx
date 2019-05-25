import * as React from "react";
import renderer from "react-test-renderer";
import { mount } from "enzyme";

import * as Modal from "./modal";

describe("UI kit", () => {
  describe("Modal", () => {
    it("render with empty nodes", () => {
      const C = Modal.createComponent();
      const root = document.createElement("div");
      const tree = renderer
        .create(
          <C dialogRoot={root} opened={false} container={{ role: "dialog" }} overlay={{ "aria-label": "overlay" }} />
        )
        .toJSON();

      expect(tree).toMatchSnapshot();
    });

    it("render with component parameters ", () => {
      const C = Modal.Component;
      const root = mount(<div />);
      const tree = mount(
        <C
          dialogRoot={root.getDOMNode() as HTMLElement}
          opened={true}
          container={{ role: "dialog" }}
          overlay={{ "aria-label": "overlay" }}
        />
      );

      expect(tree.exists("[aria-label='overlay']")).toBeTruthy;
    });

    it("render a dialog opened on a dialog root", () => {
      const C = Modal.createComponent({
        classNames: { root: "foo" },
      });
      const root = mount(<div />);
      mount(<C dialogRoot={root.getDOMNode() as HTMLElement} opened={true} />);

      expect(root.exists(".foo")).toBeTruthy;
    });
  });
});
