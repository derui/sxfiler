import * as FilerApi from "./filer-api";
import * as KeymapApi from "./keymap-api";

// define methods that are able to call to server

enum CompletionMethod {
  Setup = "completion/setup",
  Read = "completion/read",
}

export type ApiMethod = CompletionMethod | FilerApi.Methods | KeymapApi.Methods;

export const Apis = {
  Filer: FilerApi.Apis,
  Keymap: KeymapApi.Apis,
};
