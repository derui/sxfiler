// type of interaction
export enum SuggestionKind {
  Overwrite = "overwrite",
  Rename = "rename",
}

type SuggestionObject = {
  readonly kind: SuggestionKind;
  readonly nodeName: string;
};
export type Suggestion = SuggestionObject;

type SuggestionsObject = {
  readonly taskId: string;
  readonly suggestions: Suggestion[];
};

export type Suggestions = SuggestionsObject;

/**
   The factory function to create interaction from argument
 */
export const createSuggestion = (args: { kind: SuggestionKind; nodeName: string }): Suggestion => {
  return {
    ...args,
  };
};

/**
   The factory function to create suggestions from argument
 */
export const createSuggestions = (args: { taskId: string; suggestions: Suggestion[] }): Suggestions => {
  return {
    ...args,
  };
};
