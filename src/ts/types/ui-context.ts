import { ObjectEnum } from "@/utils";

// UIContext defines current context of UI.
export const UIContext = {
  OnFileTree: "onFileTree",
  OnSuggestion: "onSuggestion",
  OnCompletion: "onCompletion",
  ForHistory: "forHistory",
  ForFinder: "forFinder",
} as const;
export type UIContext = ObjectEnum<typeof UIContext>;

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
