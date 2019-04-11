import { empty, findBinding } from "./keymap";

describe("State", () => {
  describe("Keymap", () => {
    describe("findBinding", () => {
      it("return undefined when the binding related the key not found", () => {
        const state = empty();

        expect(findBinding(state, "foo")).toBeUndefined();
      });
    });
  });
});
