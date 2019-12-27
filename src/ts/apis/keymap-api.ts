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
    keymaps.getBindingsList().map(v => {
      const contexts = v.getContextsList().map(v => {
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
      return { key: v.getKey(), action: v.getAction(), when: { contexts } };
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
    const keymap = ret?.getKeymap();

    return keymap && transformKeymap(keymap);
  },
};

/**
   API definition for keymap/get
 */
const Reload: Api<Methods.Reload, void, KeymapReloadRequest, KeymapReloadResponse, Domain | undefined> = {
  method: Methods.Reload,
  parametersTransformer() {
    return new KeymapReloadRequest();
  },
  resultTransformer(ret, error) {
    if (error) {
      throw Error(error.message);
    }

    const keymap = ret?.getKeymap();

    return keymap && transformKeymap(keymap);
  },
};

export const Apis = { Get, Reload };
