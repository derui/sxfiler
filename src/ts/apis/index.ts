import * as FilerApi from "./filer-api";

// define methods that are able to call to server

enum CompletionMethod {
  Setup = "completion/setup",
  Read = "completion/read",
}

export type ApiMethod = CompletionMethod | FilerApi.Methods;

export const Apis = {
  Filer: FilerApi.Apis,
};
