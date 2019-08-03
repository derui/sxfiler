import { empty, findBinding } from "./keymap";
import { createAppContext } from "@/domains/app-context";
import { UIContext } from "@/types/ui-context";

describe("State", () => {
  describe("Keymap", () => {
    describe("findBinding", () => {
      it("return undefined when the binding related the key not found", () => {
        const state = empty();
        const context = createAppContext({ current: UIContext.OnFileTree });

        expect(findBinding(state, context, "foo")).toBeUndefined();
      });
    });
  });
});
