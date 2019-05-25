import { Suggestions } from "../domains/task-suggestion";

// Task interaction manages the state for task suggestions and replys

export type State = {
  operating: boolean;
  suggestions?: Suggestions;
  currentTaskId?: string;
  currentSuggestionIndex?: number;
};

export const empty = (): State => {
  return { operating: false };
};

// apply given suggestions to state
export const gaveSuggestions = (state: State, taskId: string, suggestions: Suggestions): State => {
  return { ...state, operating: true, currentTaskId: taskId, suggestions, currentSuggestionIndex: 0 };
};
