// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import * as E from "@/codecs/bookmark";
import { Bookmark } from "@/domains/bookmark";

export enum Methods {
  List = "bookmark/list",
  Register = "bookmark/register",
  Delete = "bookmark/delete",
}

/**
   API definition for keymap/get
 */
const List: Api<Methods.List, any, Bookmark[]> = {
  method: Methods.List,
  parametersTransformer() {
    return undefined;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return ret.map(E.encode);
  },
};

/**
   API definition for keymap/get
 */
const Register: Api<Methods.Register, { path: string }, Bookmark> = {
  method: Methods.Register,
  parametersTransformer(obj) {
    return obj;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return E.encode(ret);
  },
};

const Delete: Api<Methods.Delete, { id: string }, Bookmark> = {
  method: Methods.Delete,
  parametersTransformer(obj) {
    return obj;
  },
  resultTransformer(ret, error) {
    if (!ret && error) {
      throw Error(error.message);
    }

    return E.encode(ret);
  },
};

export const Apis = { List, Register, Delete };
