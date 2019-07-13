import { createAppContext } from "./app-context";
import UIContext from "@/types/ui-context";

describe("Domains", () => {
  describe("App Context", () => {
    it("create a object", () => {
      const obj = createAppContext({
        current: UIContext.OnCompletion,
      });

      expect(obj.current).toEqual(UIContext.OnCompletion);
      expect(obj.subContexts).toHaveLength(0);
    });

    it("return plain object", () => {
      const obj = createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });

      expect(obj.plain()).toEqual({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });
    });

    it("add sub context", () => {
      const obj = createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [],
      }).addSubContext(UIContext.ForHistory);

      expect(obj.subContexts).toContain(UIContext.ForHistory);
    });

    it("has immutable operation to add sub context", () => {
      const obj = createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [],
      });
      const newObj = obj.addSubContext(UIContext.ForHistory);

      expect(obj).not.toBe(newObj);
      expect(obj.subContexts).toHaveLength(0);
    });

    it("remove sub context", () => {
      const obj = createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      }).removeSubContext(UIContext.ForHistory);

      expect(obj.subContexts).toHaveLength(0);
    });

    it("has immutable operation to remove the sub context", () => {
      const obj = createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });
      const newObj = obj.removeSubContext(UIContext.ForHistory);

      expect(obj).not.toBe(newObj);
      expect(obj.subContexts).toHaveLength(1);
    });
  });
});
