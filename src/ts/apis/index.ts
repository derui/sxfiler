import * as FilerApi from "./filer-api";
import * as KeymapApi from "./keymap-api";
import * as TaskApi from "./task-api";
import * as CompletionApi from "./completion-api";
import * as BookmarkApi from "./bookmark-api";

export type ApiMethod =
  | CompletionApi.Methods
  | FilerApi.Methods
  | KeymapApi.Methods
  | TaskApi.Methods
  | BookmarkApi.Methods;

export const Apis = {
  Filer: FilerApi.Apis,
  Keymap: KeymapApi.Apis,
  Task: TaskApi.Apis,
  Completion: CompletionApi.Apis,
  Bookmark: BookmarkApi.Apis,
};
