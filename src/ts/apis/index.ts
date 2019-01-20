// define methods that are able to call to server

enum CompletionMethod {
  Setup = "completion/setup",
  Read = "completion/read",
}

enum FilerMethod {
  Make = "filer/make",
  Get = "filer/get",
  MoveParent = "filer/moveParent",
  EnterDirectory = "filer/enterDirectory",
  MoveNodes = "filer/moveNodes",
  DeleteNodes = "filer/deleteNodes",
}

export type ApiMethod = CompletionMethod | FilerMethod;
