// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import * as E from "@/codecs/bookmark";
import { Bookmark } from "@/domains/bookmark";
import {
  ListAllRequest,
  ListAllResponse,
  RegisterRequest,
  RegisterResponse,
  DeleteRequest,
  DeleteResponse,
} from "@/generated/bookmark_pb";

export enum Methods {
  ListAll = "bookmark/listAll",
  Register = "bookmark/register",
  Delete = "bookmark/delete",
}

/**
   API definition for keymap/get
 */
const ListAll: Api<Methods.ListAll, void, ListAllRequest, ListAllResponse, Bookmark[]> = {
  method: Methods.ListAll,
  parametersTransformer() {
    return new ListAllRequest();
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return ret?.getBookmarksList().map(E.encode) || [];
  },
};

/**
   API definition for keymap/get
 */
const Register: Api<Methods.Register, string, RegisterRequest, RegisterResponse, Bookmark> = {
  method: Methods.Register,
  parametersTransformer(path) {
    const req = new RegisterRequest();

    req.setPath(path);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    const bookmark = ret?.getBookmark();
    if (!bookmark) {
      throw Error("unknown error");
    }

    return E.encode(bookmark);
  },
};

const Delete: Api<Methods.Delete, string, DeleteRequest, DeleteResponse, Bookmark> = {
  method: Methods.Delete,
  parametersTransformer(id) {
    const req = new DeleteRequest();
    req.setId(id);
    return req;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    const bookmark = ret?.getDeletedbookmark();
    if (!bookmark) {
      throw Error("Can not get deleted bookmark");
    }

    return E.encode(bookmark);
  },
};

export const Apis = { ListAll, Register, Delete };
