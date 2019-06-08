import * as keymap from "./keymap";
import UIContext from "../types/ui-context";

describe("keymap value object", () => {
  it("can create value object without reference", () => {
    const value = [{ key: "a", action: "foo", when: { contexts: [] } }];
    const obj = keymap.createKeymap(value);

    value.push({ key: "b", action: "bar", when: { contexts: [] } });
    expect(obj.bindings).toEqual([{ key: "a", action: "foo", when: { contexts: [] } }]);
  });

  it("can find the key binding with key", () => {
    const value = [{ key: "a", action: "foo", when: { contexts: [] } }];
    const obj = keymap.createKeymap(value);

    expect(obj.find("a")).toEqual({ key: "a", action: "foo", when: { contexts: [] } });
  });

  it("shound return null when binding not found", () => {
    const value = [{ key: "a", action: "foo", when: { contexts: [] } }];
    const obj = keymap.createKeymap(value);

    expect(obj.find("b")).toBeUndefined();
  });

  it("get allowed binding in the current context", () => {
    const value = [
      { key: "a", action: "foo", when: { contexts: [] } },
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "b", action: "bar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnSuggestion] } },
    ];
    const obj = keymap.createKeymap(value).allowedWhen({ currentContext: UIContext.OnFileTree });

    expect(obj.find("a")).toEqual(value[1]);
    expect(obj.find("b")).toEqual(value[2]);
  });
});
