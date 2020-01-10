// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import { createKeymap, Keymap as Domain } from "@/domains/keymap";
import { UIContext } from "@/types/ui-context";
import {
  KeymapGetRequest,
  KeymapGetResponse,
  Keymap,
  KeymapReloadRequest,
  KeymapReloadResponse,
  Binding,
} from "../generated/keymap_pb";

export enum Methods {
  Get = "keymap/get",
  Reload = "keymap/reload",
}

/**
   Support function to transform filer from JSON representation.

   @param keymaps JSON representation for fller
   @return Filer object
 */
const transformKeymap = function transformKeymap(keymaps: Keymap): Domain {
  return createKeymap(
    keymaps.bindings.map(v => {
      const binding = Binding.create(v);
      const contexts = binding.contexts.map(v => {
        switch (v) {
          case "onFileTree":
            return UIContext.OnFileTree;
          case "onSuggestion":
            return UIContext.OnSuggestion;
          case "onCompletion":
            return UIContext.OnCompletion;
          case "forHistory":
            return UIContext.ForHistory;
          case "forFinder":
            return UIContext.ForFinder;
          default:
            throw new Error(`Unknown context: ${v}`);
        }
      });
      return { key: binding.key, action: binding.action, when: { contexts } };
    })
  );
};

/**
   API definition for keymap/get
 */
const Get: Api<Methods.Get, void, KeymapGetRequest, KeymapGetResponse, Domain | undefined> = {
  method: Methods.Get,
  parametersTransformer() {
    return new KeymapGetRequest();
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }
    const keymap = ret?.keymap;
    if (!keymap) {
      return undefined;
    }

    return transformKeymap(Keymap.create(keymap));
  },
};

/**
   API definition for keymap/get
 */
const Reload: Api<Methods.Reload, any, KeymapReloadRequest, KeymapReloadResponse, Domain | undefined> = {
  method: Methods.Reload,
  parametersTransformer() {
    return new KeymapReloadRequest();
  },
  resultTransformer(ret, error) {
    if (error) {
      throw Error(error.message);
    }

    const keymap = ret?.keymap;
    if (!keymap) {
      return undefined;
    }

    return transformKeymap(Keymap.create(keymap));
  },
};

export const Apis = { Get, Reload };
