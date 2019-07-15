import { Completion, createCompletion } from "@/domains/completion";
import { Side } from "./file-list";

export type State = {
  readonly opened: boolean;
  readonly completion: Completion;
  readonly side: Side;
};

export const empty = (): State => {
  return {
    opened: false,
    completion: createCompletion({}),
    side: Side.Left,
  };
};
