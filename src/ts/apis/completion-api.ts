// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import * as E from "@/codecs/candidate";
import { Candidate } from "@/domains/candidate";
import { SetupRequest, SetupResponse, ReadRequest, ReadResponse, Item } from "../generated/completion_pb";

export enum Methods {
  Setup = "completion/setup",
  Read = "completion/read",
}

type SourceItem = {
  id: string;
  value: string;
};

/**
   API definition for completion/setup
 */
const Setup: Api<Methods.Setup, { source: SourceItem[] }, SetupRequest, SetupResponse> = {
  method: Methods.Setup,
  parametersTransformer({ source }) {
    const items = source.map(v => {
      const item = new Item();
      item.setId(v.id);
      item.setValue(v.value);
      return item;
    });
    const req = new SetupRequest();
    req.setSourceList(items);
    return req;
  },
  resultTransformer() {
    return;
  },
};

/**
   API definition for completion/setup
 */
const Read: Api<Methods.Read, string, ReadRequest, ReadResponse, Candidate[]> = {
  method: Methods.Read,
  parametersTransformer(param) {
    const req = new ReadRequest();
    req.setInput(param);
    return req;
  },
  resultTransformer(res, error) {
    if (!res && error) {
      throw Error(error.message);
    }

    return res?.getCandidatesList().map(E.encode) || [];
  },
};

export const Apis = { Setup, Read };
