import { h } from "preact";
import { render as renderAsText } from "preact-render-to-string";
import { render, fireEvent, act } from "@testing-library/preact";
import { Component } from "./switch";

describe("UI", () => {
  describe("Switch", () => {
    test("display unchecked when property is not given", () => {
      const tree = renderAsText(<Component onChange={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    test("display unchecked when property is not given", () => {
      const tree = renderAsText(<Component checked={true} onChange={() => {}} />);

      expect(tree).toMatchSnapshot();
    });

    test("get changed value when click it", async () => {
      const callback = jest.fn();
      const query = render(<Component checked={true} onChange={callback} />);
      const root = await query.findByTestId("switch-root");

      act(() => {
        fireEvent.click(root);
      });

      expect(callback).toBeCalledWith(false);
    });

    test("get changed value if default value is not given", async () => {
      const callback = jest.fn();
      const query = render(<Component onChange={callback} />);
      const root = await query.findByTestId("switch-root");

      act(() => {
        fireEvent.click(root);
      });

      expect(callback).toBeCalledWith(true);
    });
  });
});
