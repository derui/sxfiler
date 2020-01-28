import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component } from "./scroll-bar";

describe("Project", () => {
  describe("Scroll Bar", () => {
    test("render for window", () => {
      const component = render(<Component start={1} windowSize={0.7} />);

      expect(component).toMatchSnapshot();
    });
  });
});
