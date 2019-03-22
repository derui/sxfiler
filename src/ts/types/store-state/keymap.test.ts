import { empty, findBinding } from "./keymap";

describe("State", () => {
  describe("Keymap", () => {
    describe("findBinding", () => {
      it("return null when key not found", () => {
        const state = empty();

        expect(findBinding(state, "foo")).toBeNull();
      });
    });
  });
});
