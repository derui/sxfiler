import * as keymap from "./keymap";
import UIContext from "../types/ui-context";
import { createAppContext } from "./app-context";

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
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "b", action: "bar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnSuggestion] } },
    ];
    const context = createAppContext({ current: UIContext.OnFileTree });
    const obj = keymap.createKeymap(value).allowedWhen(context);

    expect(obj.find("a")).toEqual(value[0]);
    expect(obj.find("b")).toEqual(value[1]);
  });

  it("get allowed binding in the current context and sub contexts", () => {
    const value = [
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "b", action: "bar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnSuggestion] } },
      { key: "d", action: "foobar", when: { contexts: [UIContext.ForHistory] } },
    ];
    const context = createAppContext({ current: UIContext.OnFileTree, subContexts: [UIContext.ForHistory] });
    const obj = keymap.createKeymap(value).allowedWhen(context);

    expect(obj.find("a")).toEqual(value[0]);
    expect(obj.find("b")).toEqual(value[1]);
    expect(obj.find("d")).toEqual(value[3]);
  });

  it("overwrite loose context if keymap having same key in allowed by contexts", () => {
    const value = [
      { key: "a", action: "foo", when: { contexts: [] } },
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "b", action: "bar", when: { contexts: [UIContext.OnFileTree] } },
      { key: "a", action: "foobar", when: { contexts: [UIContext.OnSuggestion] } },
      { key: "d", action: "foobar", when: { contexts: [UIContext.ForHistory] } },
    ];
    const context = createAppContext({ current: UIContext.OnFileTree, subContexts: [UIContext.ForHistory] });
    const obj = keymap.createKeymap(value).allowedWhen(context);

    expect(obj.find("a")).toEqual(value[1]);
    expect(obj.find("b")).toEqual(value[2]);
    expect(obj.find("d")).toEqual(value[4]);
  });
});
