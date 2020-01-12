export enum ReplyKind {
  Overwrite = "Overwrite",
  Rename = "Rename",
}

type OverwritePayload = {
  readonly kind: "Overwrite";
};

type RenamePayload = {
  readonly kind: "Rename";
  readonly newName: string;
};

export type ReplyPayload = OverwritePayload | RenamePayload;

export type Reply = {
  readonly reply: ReplyPayload;
  readonly taskId: string;
};

// create payload to overwrite a node
export const createOverwritePayload = function createOverwritePayload(): ReplyPayload {
  return {
    kind: ReplyKind.Overwrite,
  };
};

// create payload to rename a node
export const createRenamePayload = function createRenamePayload(newName: string): ReplyPayload {
  return {
    kind: ReplyKind.Rename,
    newName,
  };
};

// create object for reply
export const createReply = function createReply(taskId: string, payload: ReplyPayload): Reply {
  return { taskId, reply: payload };
};
