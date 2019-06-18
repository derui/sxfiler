import * as FilerApi from "./filer-api";
import * as KeymapApi from "./keymap-api";
import * as TaskApi from "./task-api";

// define methods that are able to call to server

enum CompletionMethod {
  Setup = "completion/setup",
  Read = "completion/read",
}

export type ApiMethod = CompletionMethod | FilerApi.Methods | KeymapApi.Methods | TaskApi.Methods;

export const Apis = {
  Filer: FilerApi.Apis,
  Keymap: KeymapApi.Apis,
  Task: TaskApi.Apis,
};
