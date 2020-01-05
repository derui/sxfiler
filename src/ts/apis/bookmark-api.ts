// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import * as E from "@/codecs/bookmark";
import { Bookmark as Domain } from "@/domains/bookmark";
import {
  ListAllRequest,
  ListAllResponse,
  RegisterRequest,
  RegisterResponse,
  DeleteRequest,
  DeleteResponse,
  Bookmark,
} from "../generated/bookmark_pb";

export enum Methods {
  ListAll = "bookmark/listAll",
  Register = "bookmark/register",
  Delete = "bookmark/delete",
}

/**
   API definition for keymap/get
 */
const ListAll: Api<Methods.ListAll, void, ListAllRequest, ListAllResponse, Domain[]> = {
  method: Methods.ListAll,
  parametersTransformer() {
    return new ListAllRequest();
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return ret?.bookmarks.map(v => E.encode(Bookmark.create(v))) || [];
  },
};

/**
   API definition for keymap/get
 */
const Register: Api<Methods.Register, string, RegisterRequest, RegisterResponse, Domain> = {
  method: Methods.Register,
  parametersTransformer(path) {
    const req = new RegisterRequest({ path });

    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    const bookmark = ret?.bookmark;
    if (!bookmark) {
      throw Error("unknown error");
    }

    return E.encode(Bookmark.create(bookmark));
  },
};

const Delete: Api<Methods.Delete, string, DeleteRequest, DeleteResponse, Domain> = {
  method: Methods.Delete,
  parametersTransformer(id) {
    const req = new DeleteRequest({ id });
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    const bookmark = ret?.deletedBookmark;
    if (!bookmark) {
      throw Error("Can not get deleted bookmark");
    }

    return E.encode(Bookmark.create(bookmark));
  },
};

export const Apis = { ListAll, Register, Delete };
