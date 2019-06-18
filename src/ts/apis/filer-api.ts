// defines API signature for Filer group.

import { Api } from "../libs/json-rpc/client";
import { Filer, createFiler } from "../domains/filer";
import { NodeObject, createNode } from "../domains/node";
import { createFileStat } from "../domains/file-stat";

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

const transformNode = (markedNodes: any[]): ((node: any) => NodeObject) => (node: any) => {
  const stat = createFileStat(node.stat);
  const marked = markedNodes.includes(node.id);

  return createNode({ ...node, stat, marked });
};

/**
   Support function to transform filer from JSON representation.

   @param filer JSON representation for fller
   @return Filer object
 */
const transformFiler = (filer: any): Filer => {
  if (!filer) {
    throw new Error("Filer should not be undefined or null");
  }

  const {
    id,
    fileTree: { location, nodes },
    markedNodes,
    sortOrder,
  } = filer;

  return createFiler({
    id,
    location,
    nodes: nodes.map(transformNode(markedNodes)),
    currentCursorIndex: 0,
  });
};

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
    return transformFiler(ret);
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

    return transformFiler(ret);
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

    return transformFiler(ret);
  },
};

const EnterDirectory: Api<Methods.EnterDirectory, { name: string; nodeId: string }, Filer | undefined> = {
  method: Methods.EnterDirectory,
  parametersTransformer({ name, nodeId }) {
    return { name, nodeId };
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    return transformFiler(ret);
  },
};

const ToggleMark: Api<Methods.ToggleMark, { name: string; nodeIds: string[] }, Filer | undefined> = {
  method: Methods.ToggleMark,
  parametersTransformer({ name, nodeIds }) {
    return { name, nodeIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    return transformFiler(ret);
  },
};

const Move: Api<Methods.ToggleMark, { source: string; dest: string; nodeIds: string[] }, undefined> = {
  method: Methods.ToggleMark,
  parametersTransformer({ source, dest, nodeIds }) {
    return { source, dest, nodeIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

const Copy: Api<Methods.ToggleMark, { source: string; dest: string; nodeIds: string[] }, undefined> = {
  method: Methods.ToggleMark,
  parametersTransformer({ source, dest, nodeIds }) {
    return { source, dest, nodeIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

const Delete: Api<Methods.ToggleMark, { source: string; nodeIds: string[] }, undefined> = {
  method: Methods.ToggleMark,
  parametersTransformer({ source, nodeIds }) {
    return { source, nodeIds };
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

export const Apis = { Make, Get, MoveParent, EnterDirectory, ToggleMark, Move, Copy, Delete };
