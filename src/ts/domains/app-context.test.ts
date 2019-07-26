import * as C from "./app-context";
import { UIContext } from "@/types/ui-context";
import { pipe } from "@/libs/fn";

describe("Domains", () => {
  describe("App Context", () => {
    it("create a object", () => {
      const obj = C.createAppContext({
        current: UIContext.OnCompletion,
      });

      expect(obj.current).toEqual(UIContext.OnCompletion);
      expect(obj.subContexts).toHaveLength(0);
    });

    it("return plain object", () => {
      const obj = C.createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });

      expect(obj).toEqual({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });
    });

    it("add sub context", () => {
      const obj = pipe(
        C.createAppContext,
        C.addSubContext(UIContext.ForHistory)
      )({
        current: UIContext.OnFileTree,
        subContexts: [],
      });

      expect(obj.subContexts).toContain(UIContext.ForHistory);
    });

    it("has immutable operation to add sub context", () => {
      const obj = C.createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [],
      });
      const newObj = C.addSubContext(UIContext.ForHistory)(obj);

      expect(obj).not.toBe(newObj);
      expect(obj.subContexts).toHaveLength(0);
    });

    it("remove sub context", () => {
      const obj = pipe(
        C.createAppContext,
        C.removeSubContext(UIContext.ForHistory)
      )({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });

      expect(obj.subContexts).toHaveLength(0);
    });

    it("has immutable operation to remove the sub context", () => {
      const obj = C.createAppContext({
        current: UIContext.OnFileTree,
        subContexts: [UIContext.ForHistory],
      });
      const newObj = C.removeSubContext(UIContext.ForHistory)(obj);

      expect(obj).not.toBe(newObj);
      expect(obj.subContexts).toHaveLength(1);
    });
  });
});
