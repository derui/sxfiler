import { h } from "preact";
import { render } from "preact-render-to-string";
import { Component as T } from "./mode-slot";
import { Mode, Capability } from "@/generated/filer_pb";
import { also } from "@/libs/fn";

describe("Project", () => {
  describe("Node Item", () => {
    describe("Mode slot", () => {
      it("should print name simply", () => {
        const mode = new Mode();
        const tree = render(<T mode={mode.toObject()} isDirectory={false} isSymlink={false} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print directory mode", () => {
        const mode = new Mode();
        mode.setOwner(
          also(new Capability(), (v) => {
            v.setReadable(true);
          })
        );
        const tree = render(<T mode={mode.toObject()} isDirectory={true} isSymlink={false} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print symlink mode", () => {
        const mode = new Mode();
        mode.setOwner(
          also(new Capability(), (v) => {
            v.setReadable(true);
          })
        );
        const tree = render(<T mode={mode.toObject()} isDirectory={false} isSymlink={true} />);

        expect(tree).toMatchSnapshot();
      });

      it("should print symlink when directory and symlink are true", () => {
        const mode = new Mode();
        mode.setOwner(
          also(new Capability(), (v) => {
            v.setReadable(true);
          })
        );

        const tree = render(<T mode={mode.toObject()} isDirectory={true} isSymlink={true} />);

        expect(tree).toMatchSnapshot();
      });
    });
  });
});
