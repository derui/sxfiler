import * as keymap from "./keymap";

describe("keymap value object", () => {
  it("can create value object without reference", () => {
    const value = [{ key: "a", action: "foo" }];
    const obj = keymap.createKeymap(value);

    value.push({ key: "b", action: "bar" });
    expect(obj.bindings).toEqual([{ key: "a", action: "foo" }]);
  });

  it("can find the key binding with key", () => {
    const value = [{ key: "a", action: "foo" }];
    const obj = keymap.createKeymap(value);

    expect(obj.find("a")).toEqual({ key: "a", action: "foo" });
  });

  it("shound return null when binding not found", () => {
    const value = [{ key: "a", action: "foo" }];
    const obj = keymap.createKeymap(value);

    expect(obj.find("b")).toBeUndefined();
  });
});
