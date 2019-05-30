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
export const gaveSuggestions = (state: State, suggestions: Suggestions): State => {
  return { ...state, operating: true, currentTaskId: suggestions.taskId, suggestions, currentSuggestionIndex: 0 };
};

// select suggestion to make reply
export const selectSuggestion = (state: State, index: number): State => {
  let suggestionLength = 0;
  if (state.suggestions) {
    suggestionLength = state.suggestions.suggestions.length;
  }

  return { ...state, currentSuggestionIndex: Math.min(Math.max(0, index), suggestionLength) };
};
