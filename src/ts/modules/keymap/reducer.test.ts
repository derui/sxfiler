import { reducer } from "./reducer";
import { Keymap, Binding } from "@/generated/keymap_pb";
import { actions } from "./actions";
import { UIContext } from "@/types/ui-context";
import { pipe, also } from "@/libs/fn";

describe("Modules", () => {
  describe("Keymap", () => {
    describe("Reducer", () => {
      it("update global keymap", () => {
        const keymap = new Keymap();
        const action = actions.update(keymap);

        const newState = reducer(undefined, action);
        expect(newState.globalKeymap).toEqual(keymap);
        expect(newState.currentKeymap).toEqual({});
      });

      it("update current keymap when context of binding is subset of current context", () => {
        const keymap = also(new Keymap(), (v) => {
          v.setBindingsList([
            also(new Binding(), (v) => {
              v.setKey("k");
              v.setAction("action");
              v.setContextsList([UIContext.OnFileTree]);
            }),
          ]);
        });
        const action = actions.update(keymap);

        const newState = reducer(undefined, action);
        expect(newState.globalKeymap).toEqual(keymap);
        expect(newState.currentKeymap).toEqual({});
      });

      it("add contexts and update current keymap", () => {
        const keymap = also(new Keymap(), (v) => {
          v.setBindingsList([
            also(new Binding(), (v) => {
              v.setKey("k");
              v.setAction("action");
              v.setContextsList([UIContext.OnFileTree]);
            }),
          ]);
        });
        const action = actions.update(keymap);
        const contexts = actions.addContexts([UIContext.OnFileTree]);

        const newState = pipe(reducer, (v) => reducer(v, contexts))(undefined, action);
        expect(newState.contexts).toEqual(new Set([UIContext.OnFileTree]));
        expect(newState.currentKeymap).toEqual({ k: "action" });
      });

      it("remove contexts and update current keymap", () => {
        const keymap = also(new Keymap(), (v) => {
          v.setBindingsList([
            also(new Binding(), (v) => {
              v.setKey("k");
              v.setAction("action");
              v.setContextsList([UIContext.OnFileTree]);
            }),
          ]);
        });
        const action = actions.update(keymap);
        const addContexts = actions.addContexts([UIContext.OnFileTree]);
        const removeContexts = actions.removeContexts([UIContext.OnFileTree]);

        const newState = pipe(
          reducer,
          (v) => reducer(v, addContexts),
          (v) => reducer(v, removeContexts)
        )(undefined, action);
        expect(newState.contexts).toEqual(new Set([]));
        expect(newState.currentKeymap).toEqual({});
      });
    });
  });
});
