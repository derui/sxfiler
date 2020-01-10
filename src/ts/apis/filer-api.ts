// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import { Filer as Domain } from "@/domains/filer";
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
  Filer,
  IFilerMakeResponse,
  IFilerGetResponse,
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
const Make: Api<
  Methods.Make,
  { initialLocation: string; name: string },
  FilerMakeRequest,
  IFilerMakeResponse,
  Domain
> = {
  method: Methods.Make,
  parametersTransformer({ initialLocation, name }) {
    const req = new FilerMakeRequest({
      name,
      initialLocation,
    });
    return req;
  },
  resultTransformer(ret, error) {
    if (error) {
      throw Error(error.message);
    }

    const filer = ret?.filer;
    if (!filer) {
      throw Error("unknown error");
    }

    return E.encode(Filer.create(filer));
  },
};

const Get: Api<Methods.Get, string, FilerGetRequest, IFilerGetResponse, Domain | undefined> = {
  method: Methods.Get,
  parametersTransformer(name) {
    const req = new FilerGetRequest({ name });
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    const filer = ret?.filer;
    if (!filer) {
      return undefined;
    }

    return E.encode(Filer.create(filer));
  },
};

const MoveParent: Api<
  Methods.MoveParent,
  string,
  FilerMoveParentRequest,
  FilerMoveParentResponse,
  Domain | undefined
> = {
  method: Methods.MoveParent,
  parametersTransformer(name) {
    const req = new FilerMoveParentRequest({ name });
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    const filer = ret?.filer;
    if (!filer) {
      return undefined;
    }

    return E.encode(Filer.create(filer));
  },
};

const EnterDirectory: Api<
  Methods.EnterDirectory,
  { name: string; itemId: string },
  FilerEnterDirectoryRequest,
  FilerEnterDirectoryResponse,
  Domain | undefined
> = {
  method: Methods.EnterDirectory,
  parametersTransformer({ name, itemId }) {
    const request = new FilerEnterDirectoryRequest({
      name,
      itemId,
    });
    return request;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }
    const filer = ret?.filer;
    if (!filer) {
      return undefined;
    }

    return E.encode(Filer.create(filer));
  },
};

const ToggleMark: Api<
  Methods.ToggleMark,
  { name: string; itemIds: string[] },
  FilerToggleMarkRequest,
  FilerToggleMarkResponse,
  Domain | undefined
> = {
  method: Methods.ToggleMark,
  parametersTransformer({ name, itemIds }) {
    const req = new FilerToggleMarkRequest({
      name,
      itemIds,
    });
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error && error.hasCode(-2)) {
      return undefined;
    }

    const filer = ret?.filer;
    if (!filer) {
      return undefined;
    }

    return E.encode(Filer.create(filer));
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
    const req = new FilerMoveRequest({
      source,
      dest,
      itemIds,
    });
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
    const req = new FilerCopyRequest({
      source,
      dest,
      itemIds,
    });
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
    const req = new FilerDeleteRequest({
      source,
      itemIds,
    });
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
  Domain | undefined
> = {
  method: Methods.Jump,
  parametersTransformer({ location, name }) {
    const req = new FilerJumpLocationRequest({
      location,
      name,
    });
    return req;
  },
  resultTransformer(ret, error) {
    if (error) {
      throw Error(error.message);
    }

    const filer = ret?.filer;
    if (!filer) {
      return undefined;
    }

    return E.encode(Filer.create(filer));
  },
};

export const Apis = { Make, Get, MoveParent, EnterDirectory, ToggleMark, Move, Copy, Delete, Jump };
