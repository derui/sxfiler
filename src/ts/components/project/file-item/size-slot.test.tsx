import bigInt from "big-integer";
import * as React from "react";
import renderer from "react-test-renderer";
import T from "./size-slot";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Size slot", () => {
      it("should print zero size", () => {
        const tree = renderer.create(<T size={bigInt(0)} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print byte size when size less than 1024", () => {
        const tree = renderer.create(<T size={bigInt(1023)} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print kibibyte size when size between 1KiB and 1023KiB", () => {
        const tree = renderer.create(<T size={bigInt(1024 * 2)} />).toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print MiB size when size between 1MiB and 1023MiB", () => {
        const tree = renderer
          .create(
            <T
              size={bigInt(1024)
                .multiply(bigInt(1024))
                .multiply(bigInt(3))}
            />
          )
          .toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print GiB size when size between 1GiB and 1023GiB", () => {
        const tree = renderer
          .create(
            <T
              size={bigInt(1024)
                .multiply(bigInt(1024))
                .multiply(bigInt(1024))
                .multiply(bigInt(5))}
            />
          )
          .toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print TiB size when size between 1TiB and 1023TiB", () => {
        const tree = renderer
          .create(
            <T
              size={bigInt(1024)
                .multiply(bigInt(1024))
                .multiply(bigInt(1024))
                .multiply(bigInt(1024))
                .multiply(bigInt(5))}
            />
          )
          .toJSON();

        expect(tree).toMatchSnapshot();
      });

      it("should print hyphen only when unit can not calculate", () => {
        const tree = renderer
          .create(
            <T
              size={bigInt(1024)
                .multiply(bigInt(1024))
                .multiply(bigInt(1024))
                .multiply(bigInt(1024))
                .multiply(bigInt(1024))}
            />
          )
          .toJSON();

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
