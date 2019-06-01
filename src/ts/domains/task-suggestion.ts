// type of interaction
export enum SuggestionKind {
  Overwrite = "overwrite",
  Rename = "rename",
}

type SuggestionObject = {
  readonly kind: SuggestionKind;
};
export type Suggestion = SuggestionObject;

type SuggestionsObject = {
  readonly taskId: string;
  readonly nodeName: string;
  readonly suggestions: Suggestion[];
};

export type Suggestions = SuggestionsObject;

/**
   The factory function to create interaction from argument
 */
export const createSuggestion = (args: { kind: SuggestionKind }): Suggestion => {
  return {
    ...args,
  };
};

/**
   The factory function to create suggestions from argument
 */
export const createSuggestions = (args: {
  taskId: string;
  nodeName: string;
  suggestions: Suggestion[];
}): Suggestions => {
  return {
    ...args,
  };
};
