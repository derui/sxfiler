import * as React from "react";
import { mount } from "enzyme";

import * as Modal from "./modal";

const Container = ({ role, className }: any) => {
  return <div className={className} role={role} />;
};

describe("UI kit", () => {
  describe("Modal", () => {
    it("render with empty nodes", () => {
      const C = Modal.createComponent({
        container: Container,
      });
      const root = document.createElement("div");
      const tree = mount(<C dialogRoot={root} opened={false} overlay={{ "aria-label": "overlay" }} />);

      expect(tree.exists()).toBeTruthy;
    });

    it("render with component parameters ", () => {
      const C = Modal.createComponent({
        container: Container,
      });
      const root = mount(<div />);
      const tree = mount(
        <C dialogRoot={root.getDOMNode() as HTMLElement} opened={true} overlay={{ "aria-label": "overlay" }} />
      );

      expect(tree.exists("[aria-label='overlay']")).toBeTruthy;
    });

    it("render a dialog opened on a dialog root", () => {
      const C = Modal.createComponent({
        classNames: { root: "foo" },
        container: Container,
      });
      const root = mount(<div />);
      mount(<C dialogRoot={root.getDOMNode() as HTMLElement} opened={true} />);

      expect(root.exists(".foo")).toBeTruthy;
    });
  });
});
