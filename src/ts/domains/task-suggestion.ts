// type of interaction
export enum SuggestionKind {
  Overwrite = "overwrite",
  Rename = "rename",
}

export type Suggestions = {
  readonly taskId: string;
  readonly itemName: string;
  readonly suggestions: SuggestionKind[];
};

/**
   The factory function to create suggestions from argument
 */
export const createSuggestions = function createSuggestions(args: {
  taskId: string;
  itemName: string;
  suggestions: SuggestionKind[];
}): Suggestions {
  return {
    ...args,
  };
};
