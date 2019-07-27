// type of interaction
export enum SuggestionKind {
  Overwrite = "overwrite",
  Rename = "rename",
}

export type Suggestion = {
  readonly kind: SuggestionKind;
};

export type Suggestions = {
  readonly taskId: string;
  readonly nodeName: string;
  readonly suggestions: Suggestion[];
};

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
