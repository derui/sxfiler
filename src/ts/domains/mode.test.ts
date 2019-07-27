import { createCapability, emptyCapability, allowToWrite, allowToRead, allowToExecute } from "./capability";
import { createMode, emptyMode, changeGroup, changeOwner, changeOthers } from "./mode";

describe("Domain", () => {
  describe("Mode", () => {
    describe("factory", () => {
      it("return the mode that setted all capability", () => {
        const empty = emptyCapability();
        const mode = createMode({
          owner: allowToWrite(empty),
          group: allowToRead(empty),
          others: allowToExecute(empty),
        });

        expect(mode.owner).toEqual(allowToWrite(empty));
        expect(mode.group).toEqual(allowToRead(empty));
        expect(mode.others).toEqual(allowToExecute(empty));
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
      it("get plain object from changed object", () => {
        const mode = changeGroup(allowToExecute(emptyCapability()))(emptyMode());

        expect(mode.owner).toEqual(emptyCapability());
        expect(mode.group).toEqual(allowToExecute(emptyCapability()));
        expect(mode.others).toEqual(emptyCapability());
      });

      it("should be able to create empty mode", () => {
        const mode = emptyMode();
        const cap = createCapability({ writable: false, readable: false, executable: false });

        expect(mode.owner).toEqual(cap);
        expect(mode.group).toEqual(cap);
        expect(mode.others).toEqual(cap);
      });

      it("should be able to change capability of owner", () => {
        const mode = emptyMode();
        const empty = emptyCapability();
        const newMode = changeOwner(allowToWrite(empty))(mode);

        expect(newMode.owner).toEqual(allowToWrite(empty));
        expect(newMode.group).toEqual(mode.group);
        expect(newMode.others).toEqual(mode.others);
      });

      it("should be able to change capability of group", () => {
        const mode = emptyMode();
        const capability = allowToRead(emptyCapability());
        const newMode = changeGroup(capability)(mode);

        expect(newMode.group).toEqual(capability);
        expect(newMode.owner).toEqual(mode.owner);
        expect(newMode.others).toEqual(mode.others);
      });

      it("should be able to change capability of others", () => {
        const mode = emptyMode();
        const capability = allowToExecute(emptyCapability());
        const newMode = changeOthers(capability)(mode);

        expect(newMode.others).toEqual(capability);
        expect(newMode.owner).toEqual(mode.owner);
        expect(newMode.group).toEqual(mode.group);
      });
    });
  });
});
