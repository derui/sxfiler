// defines API signature for Filer group.

import { Api } from "../libs/json-rpc/client";
import { Filer } from "../domains/filer";
import FilerFactory from "../domains/filer-factory";
import FileStatFactory from "../domains/file-stat-factory";
import { Node, createNode } from "../domains/node";

export enum Methods {
  Make = "filer/make",
  Get = "filer/get",
  MoveParent = "filer/moveParent",
  EnterDirectory = "filer/enterDirectory",
  MoveNodes = "filer/moveNodes",
  DeleteNodes = "filer/deleteNodes",
}

const transformNode = (node: any): Node => {
  const stat = FileStatFactory.create(node.stat);

  return createNode({ ...node, stat });
};

/**
   Support function to transform filer from JSON representation.

   @param filer JSON representation for fller
   @return Filer object
 */
function transformFiler(filer: any): Filer {
  if (!filer) {
    throw new Error("Filer should not be undefined or null");
  }

  const {
    id,
    fileTree: { location, nodes },
    selectedNodes,
    sortOrder,
  } = filer;

  return FilerFactory.create({
    id,
    location,
    nodes: nodes.map(transformNode),
  });
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

export const Apis = { Make, Get };
