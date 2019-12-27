// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import { Filer } from "@/domains/filer";
import * as E from "@/codecs/filer";
import {
  FilerMakeRequest,
  FilerMakeResponse,
  FilerGetRequest,
  FilerGetResponse,
  FilerMoveParentRequest,
  FilerMoveParentResponse,
  FilerEnterDirectoryRequest,
  FilerEnterDirectoryResponse,
  FilerToggleMarkRequest,
  FilerToggleMarkResponse,
  FilerMoveRequest,
  FilerMoveResponse,
  FilerCopyRequest,
  FilerCopyResponse,
  FilerDeleteRequest,
  FilerDeleteResponse,
  FilerJumpLocationRequest,
  FilerJumpLocationResponse,
} from "../generated/filer_pb";

export enum Methods {
  Make = "filer/make",
  Get = "filer/get",
  MoveParent = "filer/moveParent",
  EnterDirectory = "filer/enterDirectory",
  ToggleMark = "filer/toggleMark",
  Move = "filer/move",
  Copy = "filer/copy",
  Delete = "filer/delete",
  Jump = "filer/jump",
}

/**
   API definition for filer/make
 */
const Make: Api<Methods.Make, { initialLocation: string; name: string }, FilerMakeRequest, FilerMakeResponse, Filer> = {
  method: Methods.Make,
  parametersTransformer({ initialLocation, name }) {
    const req = new FilerMakeRequest();
    req.setName(name);
    req.setInitiallocation(initialLocation);
    return req;
  },
  resultTransformer(ret, error) {
    if (error) {
      throw Error(error.message);
    }

    const filer = ret?.getFiler();
    if (!filer) {
      throw Error("unknown error");
    }

    return E.encode(filer);
  },
};

const Get: Api<Methods.Get, string, FilerGetRequest, FilerGetResponse, Filer | undefined> = {
  method: Methods.Get,
  parametersTransformer(name) {
    const req = new FilerGetRequest();
    req.setName(name);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    const filer = ret?.getFiler();

    return filer && E.encode(filer);
  },
};

const MoveParent: Api<
  Methods.MoveParent,
  string,
  FilerMoveParentRequest,
  FilerMoveParentResponse,
  Filer | undefined
> = {
  method: Methods.MoveParent,
  parametersTransformer(name) {
    const req = new FilerMoveParentRequest();
    req.setName(name);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    const filer = ret?.getFiler();

    return filer && E.encode(filer);
  },
};

const EnterDirectory: Api<
  Methods.EnterDirectory,
  { name: string; itemId: string },
  FilerEnterDirectoryRequest,
  FilerEnterDirectoryResponse,
  Filer | undefined
> = {
  method: Methods.EnterDirectory,
  parametersTransformer({ name, itemId }) {
    const request = new FilerEnterDirectoryRequest();
    request.setName(name);
    request.setItemid(itemId);
    return request;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }
    const filer = ret?.getFiler();

    return filer && E.encode(filer);
  },
};

const ToggleMark: Api<
  Methods.ToggleMark,
  { name: string; itemIds: string[] },
  FilerToggleMarkRequest,
  FilerToggleMarkResponse,
  Filer | undefined
> = {
  method: Methods.ToggleMark,
  parametersTransformer({ name, itemIds }) {
    const req = new FilerToggleMarkRequest();
    req.setName(name);
    req.setItemidsList(itemIds);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    const filer = ret?.getFiler();

    return filer && E.encode(filer);
  },
};

const Move: Api<
  Methods.Move,
  { source: string; dest: string; itemIds: string[] },
  FilerMoveRequest,
  FilerMoveResponse,
  undefined
> = {
  method: Methods.Move,
  parametersTransformer({ source, dest, itemIds }) {
    const req = new FilerMoveRequest();
    req.setSource(source);
    req.setDest(dest);
    req.setItemidsList(itemIds);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

const Copy: Api<
  Methods.Copy,
  { source: string; dest: string; itemIds: string[] },
  FilerCopyRequest,
  FilerCopyResponse,
  undefined
> = {
  method: Methods.Copy,
  parametersTransformer({ source, dest, itemIds }) {
    const req = new FilerCopyRequest();
    req.setSource(source);
    req.setDest(dest);
    req.setItemidsList(itemIds);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return undefined;
  },
};

const Delete: Api<Methods.Delete, { source: string; itemIds: string[] }, FilerDeleteRequest, FilerDeleteResponse> = {
  method: Methods.Delete,
  parametersTransformer({ source, itemIds }) {
    const req = new FilerDeleteRequest();
    req.setSource(source);
    req.setItemidsList(itemIds);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw new Error(`Error occuerred: ${error}`);
    }

    return ret;
  },
};

/**
   API definition for filer/make
 */
const Jump: Api<
  Methods.Jump,
  {
    location: string;
    name: string;
  },
  FilerJumpLocationRequest,
  FilerJumpLocationResponse,
  Filer | undefined
> = {
  method: Methods.Jump,
  parametersTransformer({ location, name }) {
    const req = new FilerJumpLocationRequest();
    req.setLocation(location);
    req.setName(name);
    return req;
  },
  resultTransformer(ret, error) {
    if (error) {
      throw Error(error.message);
    }

    const filer = ret?.getFiler();

    return filer && E.encode(filer);
  },
};

export const Apis = { Make, Get, MoveParent, EnterDirectory, ToggleMark, Move, Copy, Delete, Jump };
