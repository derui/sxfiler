// UIContext defines current context of UI.
export enum UIContext {
  OnFileTree = "onFileTree",
  OnSuggestion = "onSuggestion",
  OnCompletion = "onCompletion",
  ForHistory = "forHistory",
  ForFinder = "forFinder",
}

/**
 * convert string to UIContext.
 */
export const toUIContext = function toUIContext(value: string): UIContext | undefined {
  for (let v of Object.entries(UIContext)) {
    if (v[1] === value) {
      return v[1];
    }
  }

  return undefined;
};
