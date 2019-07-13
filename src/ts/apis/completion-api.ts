// defines API signature for Filer group.

import { Api } from "@/libs/json-rpc/client";
import * as E from "@/codecs/candidate";
import { Candidate } from "@/domains/candidate";

export enum Methods {
  Setup = "completion/setup",
  Read = "completion/read",
}

type Item = {
  id: string;
  value: string;
};

/**
   API definition for completion/setup
 */
const Setup: Api<
  Methods.Setup,
  {
    source: Item[];
  }
> = {
  method: Methods.Setup,
  parametersTransformer: params => params,
  resultTransformer() {
    return;
  },
};

/**
   API definition for completion/setup
 */
const Read: Api<
  Methods.Read,
  {
    input: string;
  },
  Candidate[]
> = {
  method: Methods.Read,
  parametersTransformer: params => params,
  resultTransformer(res) {
    return res.map(E.encode);
  },
};

export const Apis = { Setup, Read };
