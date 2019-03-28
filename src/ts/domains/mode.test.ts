import { createCapability, emptyCapability } from "./capability";
import { createMode, emptyMode } from "./mode";

describe("Domain", () => {
  describe("Mode", () => {
    describe("factory", () => {
      it("return the mode that setted all capability", () => {
        const mode = createMode({
          owner: emptyCapability().allowToWrite(),
          group: emptyCapability().allowToRead(),
          others: emptyCapability().allowToExecute(),
        });

        expect(mode.owner).toEqual(emptyCapability().allowToWrite());
        expect(mode.group).toEqual(emptyCapability().allowToRead());
        expect(mode.others).toEqual(emptyCapability().allowToExecute());
      });
    });

    describe("empty factory", () => {
      it("return the mode that is disabled all capabilities", () => {
        const mode = emptyMode();

        expect(mode.owner).toEqual(emptyCapability());
        expect(mode.group).toEqual(emptyCapability());
        expect(mode.others).toEqual(emptyCapability());
      });
    });

    describe("methods", () => {
      it("should be able to create empty mode", () => {
        const mode = emptyMode();
        const cap = createCapability({ writable: false, readable: false, executable: false });

        expect(mode.owner).toEqual(cap);
        expect(mode.group).toEqual(cap);
        expect(mode.others).toEqual(cap);
      });

      it("should be able to change capability of owner", () => {
        const mode = emptyMode();
        const newMode = mode.changeOwner(emptyCapability().allowToWrite());

        expect(newMode.owner).toStrictEqual(emptyCapability().allowToWrite());
        expect(newMode.group).toEqual(mode.group);
        expect(newMode.others).toEqual(mode.others);
      });

      it("should be able to change capability of group", () => {
        const mode = emptyMode();
        const newMode = mode.changeGroup(emptyCapability().allowToRead());

        expect(newMode.group).toStrictEqual(emptyCapability().allowToRead());
        expect(newMode.owner).toEqual(mode.owner);
        expect(newMode.others).toEqual(mode.others);
      });

      it("should be able to change capability of others", () => {
        const mode = emptyMode();
        const newMode = mode.changeOthers(emptyCapability().allowToExecute());

        expect(newMode.others).toStrictEqual(emptyCapability().allowToExecute());
        expect(newMode.owner).toEqual(mode.owner);
        expect(newMode.group).toEqual(mode.group);
      });
    });
  });
});
