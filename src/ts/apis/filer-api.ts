// defines API signature for Filer group.

import { Api } from "../libs/json-rpc/client";
import { Filer } from "../domains/filer";
import * as E from "../codecs/filer";

export enum Methods {
  Make = "filer/make",
  Get = "filer/get",
  MoveParent = "filer/moveParent",
  EnterDirectory = "filer/enterDirectory",
  ToggleMark = "filer/toggleMark",
  Move = "filer/move",
  Copy = "filer/copy",
  Delete = "filer/delete",
}

/**
   API definition for filer/make
 */
const Make: Api<
  Methods.Make,
  {
    initialLocation: string;
    name: string;
  },
  Filer
> = {
  method: Methods.Make,
  parametersTransformer: params => params,
  resultTransformer(ret) {
    return E.encode(ret);
  },
};

const Get: Api<Methods.Get, string, Filer | undefined> = {
  method: Methods.Get,
  parametersTransformer(name) {
    return { name };
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    return E.encode(ret);
  },
};

const MoveParent: Api<Methods.MoveParent, string, Filer | undefined> = {
  method: Methods.MoveParent,
  parametersTransformer(name) {
    return { name };
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    return E.encode(ret);
  },
};

const EnterDirectory: Api<Methods.EnterDirectory, { name: string; itemId: string }, Filer | undefined> = {
  method: Methods.EnterDirectory,
  parametersTransformer({ name, itemId }) {
    return { name, itemId };
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    return E.encode(ret);
  },
};

const ToggleMark: Api<Methods.ToggleMark, { name: string; itemIds: string[] }, Filer | undefined> = {
  method: Methods.ToggleMark,
  parametersTransformer({ name, itemIds }) {
    return { name, itemIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    return E.encode(ret);
  },
};

const Move: Api<Methods.Move, { source: string; dest: string; itemIds: string[] }, undefined> = {
  method: Methods.Move,
  parametersTransformer({ source, dest, itemIds }) {
    return { source, dest, itemIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

const Copy: Api<Methods.Copy, { source: string; dest: string; itemIds: string[] }, undefined> = {
  method: Methods.Copy,
  parametersTransformer({ source, dest, itemIds }) {
    return { source, dest, itemIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

const Delete: Api<Methods.Delete, { source: string; itemIds: string[] }> = {
  method: Methods.Delete,
  parametersTransformer({ source, itemIds }) {
    return { source, itemIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return ret;
  },
};

export const Apis = { Make, Get, MoveParent, EnterDirectory, ToggleMark, Move, Copy, Delete };
