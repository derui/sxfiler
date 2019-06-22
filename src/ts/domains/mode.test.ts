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
        const plain = mode.plain();

        expect(plain.owner).toEqual(
          emptyCapability()
            .allowToWrite()
            .plain()
        );

        expect(plain.group).toEqual(
          emptyCapability()
            .allowToRead()
            .plain()
        );
        expect(plain.others).toEqual(
          emptyCapability()
            .allowToExecute()
            .plain()
        );
      });
    });

    describe("empty factory", () => {
      it("return the mode that is disabled all capabilities", () => {
        const mode = emptyMode().plain();

        expect(mode.owner).toEqual(emptyCapability().plain());
        expect(mode.group).toEqual(emptyCapability().plain());
        expect(mode.others).toEqual(emptyCapability().plain());
      });
    });

    describe("methods", () => {
      it("get plain version mode", () => {
        const mode = emptyMode();

        const plain = mode.plain();

        expect(plain.owner).toEqual(emptyCapability().plain());
        expect(plain.group).toEqual(emptyCapability().plain());
        expect(plain.others).toEqual(emptyCapability().plain());
      });

      it("get plain object from changed object", () => {
        const mode = emptyMode()
          .changeGroup(emptyCapability().allowToExecute())
          .plain();

        expect(mode.owner).toEqual(emptyCapability().plain());
        expect(mode.group).toEqual(
          emptyCapability()
            .allowToExecute()
            .plain()
        );
        expect(mode.others).toEqual(emptyCapability().plain());
      });

      it("should be able to create empty mode", () => {
        const mode = emptyMode().plain();
        const cap = createCapability({ writable: false, readable: false, executable: false }).plain();

        expect(mode.owner).toEqual(cap);
        expect(mode.group).toEqual(cap);
        expect(mode.others).toEqual(cap);
      });

      it("should be able to change capability of owner", () => {
        const mode = emptyMode();
        const newMode = mode.changeOwner(emptyCapability().allowToWrite());

        expect(newMode.owner.plain()).toEqual(
          emptyCapability()
            .allowToWrite()
            .plain()
        );
        expect(newMode.group).toEqual(mode.group);
        expect(newMode.others).toEqual(mode.others);
      });

      it("should be able to change capability of group", () => {
        const mode = emptyMode();
        const newMode = mode.changeGroup(emptyCapability().allowToRead());

        expect(newMode.group.plain()).toEqual(
          emptyCapability()
            .allowToRead()
            .plain()
        );
        expect(newMode.owner).toEqual(mode.owner);
        expect(newMode.others).toEqual(mode.others);
      });

      it("should be able to change capability of others", () => {
        const mode = emptyMode();
        const newMode = mode.changeOthers(emptyCapability().allowToExecute());

        expect(newMode.others.plain()).toEqual(
          emptyCapability()
            .allowToExecute()
            .plain()
        );
        expect(newMode.owner).toEqual(mode.owner);
        expect(newMode.group).toEqual(mode.group);
      });
    });
  });
});
