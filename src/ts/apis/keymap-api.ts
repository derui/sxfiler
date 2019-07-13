// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import { createKeymap, Keymap } from "@/domains/keymap";
import { UIContext } from "@/types/ui-context";

export enum Methods {
  Get = "keymap/get",
  Reload = "keymap/reload",
}

/**
   Support function to transform filer from JSON representation.

   @param keymaps JSON representation for fller
   @return Filer object
 */
function transformKeymap(keymaps: {
  bindings: [{ key: string; action: string; when: { contexts: string[] } }];
}): Keymap {
  if (!keymaps) {
    throw new Error("Keymap should not be undefined or null");
  }

  return createKeymap(
    keymaps.bindings.map(v => {
      const contexts = v.when.contexts.map(v => {
        switch (v) {
          case UIContext.OnFileTree:
            return UIContext.OnFileTree;
          case UIContext.OnSuggestion:
            return UIContext.OnSuggestion;
          default:
            throw new Error(`Unknown context: ${v}`);
        }
      });
      return { ...v, when: { contexts } };
    })
  );
}

/**
   API definition for keymap/get
 */
const Get: Api<Methods.Get, any, Keymap> = {
  method: Methods.Get,
  parametersTransformer() {
    return undefined;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return transformKeymap(ret);
  },
};

/**
   API definition for keymap/get
 */
const Reload: Api<Methods.Reload, any, Keymap> = {
  method: Methods.Reload,
  parametersTransformer() {
    return undefined;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return transformKeymap(ret);
  },
};

export const Apis = { Get, Reload };
