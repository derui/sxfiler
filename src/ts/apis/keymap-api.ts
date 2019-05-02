// defines API signature for Filer group.

import { Api } from "../libs/json-rpc/client";
import { createKeymap, Keymap } from "../domains/keymap";

export enum Methods {
  Get = "keymap/get",
  Reload = "keymap/reload",
  AddContext = "keymap/addContext",
  DeleteContext = "keymap/deleteContext",
}

/**
   Support function to transform filer from JSON representation.

   @param keymaps JSON representation for fller
   @return Filer object
 */
function transformKeymap(keymaps: { bindings: [{ key: string; action: string }] }): Keymap {
  if (!keymaps) {
    throw new Error("Keymap should not be undefined or null");
  }

  return createKeymap(keymaps.bindings);
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

/**
   API definition for keymap/addContext
 */
const AddContext: Api<Methods.AddContext, { context: string }, Keymap> = {
  method: Methods.AddContext,
  parametersTransformer({ context }) {
    return { context };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return transformKeymap(ret);
  },
};

/**
   API definition for keymap/deleteContext
 */
const DeleteContext: Api<Methods.DeleteContext, { context: string }, Keymap> = {
  method: Methods.DeleteContext,
  parametersTransformer({ context }) {
    return { context };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return transformKeymap(ret);
  },
};

export const Apis = { Get, AddContext, DeleteContext, Reload };
