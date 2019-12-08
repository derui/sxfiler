import { Suggestions, SuggestionKind } from "@/domains/task-suggestion";
import { Reply, ReplyPayload, createOverwritePayload, createRenamePayload, createReply } from "@/domains/task-reply";

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
  currentReply(): Reply | undefined;
};

const currentReply = function currentReply(this: StateObject): Reply | undefined {
  const taskId = this.currentTaskId;

  if (this.currentReplyIndex === undefined || !this.replies || !taskId) {
    return undefined;
  }
  return createReply(taskId, this.replies[this.currentReplyIndex]);
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
const suggestionToReplyPayload = (itemName: string) => (obj: SuggestionKind): ReplyPayload => {
  switch (obj) {
    case SuggestionKind.Overwrite:
      return createOverwritePayload();
    case SuggestionKind.Rename:
      return createRenamePayload(itemName);
  }
};

// apply given suggestions to state
export const giveSuggestions = function giveSuggestions(state: State, suggestions: Suggestions): State {
  const replies = suggestions.suggestions.map(suggestionToReplyPayload(suggestions.itemName));
  if (state.currentTaskId) {
    return { ...state, replyQueue: state.replyQueue.concat(replies) };
  }

  return { ...state, operating: true, currentTaskId: suggestions.taskId, replies, currentReplyIndex: 0 };
};

// Update state when the task finished
export const finishTask = function finishTask(state: State, taskId: string): State {
  if (state.currentTaskId !== taskId) {
    return state;
  }

  return {
    ...state,
    replyQueue: state.replyQueue.splice(1),
    operating: false,
    currentTaskId: undefined,
  };
};

// Cancel the task
export const cancelTask = function cancelTask(state: State, taskId: string): State {
  if (state.currentTaskId !== taskId) {
    return state;
  }

  return {
    ...state,
    operating: false,
    currentTaskId: undefined,
  };
};

// select reply
export const selectReply = function selectReply(state: State, index: number): State {
  let repliesLength = 0;
  if (state.replies) {
    repliesLength = state.replies.length;
  }

  return { ...state, currentReplyIndex: Math.min(Math.max(0, index), repliesLength - 1) };
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
