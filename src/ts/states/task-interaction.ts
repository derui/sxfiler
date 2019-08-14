import { Suggestions, Suggestion, SuggestionKind } from "@/domains/task-suggestion";
import { ReplyPayload, createOverwritePayload, createRenamePayload } from "@/domains/task-reply";

// Task interaction manages the state for task suggestions and replys

type Replies = ReplyPayload[];

type StateObject = {
  readonly operating: boolean;
  readonly replies?: Replies;
  readonly currentTaskId?: string;
  readonly currentReplyIndex?: number;
  readonly replyQueue: Replies[];
};

export type State = StateObject & {
  currentReply(): ReplyPayload | undefined;
};

const currentReply = function currentReply(this: StateObject): ReplyPayload | undefined {
  if (this.currentReplyIndex === undefined || !this.replies) {
    return undefined;
  }
  return this.replies[this.currentReplyIndex];
};

// create state internally
const createState = function createState(obj: StateObject): State {
  return {
    ...obj,
    currentReply,
  };
};

export const empty = function empty(): State {
  return createState({
    operating: false,
    replyQueue: [],
  });
};

// convert the suggestion to the payload of reply
const suggestionToReplyPayload = (nodeName: string) => (obj: Suggestion): ReplyPayload => {
  switch (obj.kind) {
    case SuggestionKind.Overwrite:
      return createOverwritePayload();
    case SuggestionKind.Rename:
      return createRenamePayload(nodeName);
  }
};

// apply given suggestions to state
export const giveSuggestions = function giveSuggestions(state: State, suggestions: Suggestions): State {
  const replies = suggestions.suggestions.map(suggestionToReplyPayload(suggestions.nodeName));
  if (state.currentTaskId) {
    return { ...state, replyQueue: state.replyQueue.concat(replies) };
  }

  return { ...state, operating: true, currentTaskId: suggestions.taskId, replies, currentReplyIndex: 0 };
};

// select reply
export const selectReply = function selectReply(state: State, index: number): State {
  let repliesLength = 0;
  if (state.replies) {
    repliesLength = state.replies.length;
  }

  return { ...state, currentReplyIndex: Math.min(Math.max(0, index), repliesLength) };
};

export const updateCurrentReply = function updateCurrentReply(state: State, payload: ReplyPayload): State {
  if (!state.replies) {
    return state;
  }

  const v = Array.from(state.replies);

  if (state.currentReplyIndex === undefined || v[state.currentReplyIndex].kind !== payload.kind) {
    return state;
  }

  v[state.currentReplyIndex] = payload;

  return { ...state, replies: v };
};
