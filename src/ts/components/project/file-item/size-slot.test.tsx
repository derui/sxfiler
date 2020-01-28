import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component as T } from "./size-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Size slot", () => {
      const kb = BigInt(1024);
      it("should print zero size", () => {
        const tree = render(<T size={BigInt(0)} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print byte size when size less than 1024", () => {
        const tree = render(<T size={BigInt(1023)} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print kibibyte size when size between 1KiB and 1023KiB", () => {
        const tree = render(<T size={BigInt(1024) * BigInt(2)} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print MiB size when size between 1MiB and 1023MiB", () => {
        const tree = render(<T size={kb * kb * BigInt(3)} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print GiB size when size between 1GiB and 1023GiB", () => {
        const tree = render(<T size={kb * kb * kb * BigInt(5)} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print TiB size when size between 1TiB and 1023TiB", () => {
        const tree = render(<T size={kb * kb * kb * kb * BigInt(5)} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print hyphen only when unit can not calculate", () => {
        const tree = render(<T size={kb * kb * kb * kb * kb} />);

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
