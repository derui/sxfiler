type OverwritePayload = {
  readonly kind: "overwrite";
};

type RenamePayload = {
  readonly kind: "rename";
  readonly newName: string;
};

type ReplyPayloadObject = OverwritePayload | RenamePayload;
export type ReplyPayload = ReplyPayloadObject;

type ReplyObject = {
  readonly reply: ReplyPayload;
  readonly taskId: string;
};
export type Reply = ReplyObject;

// create payload to overwrite a node
export const createOverwritePayload = (): ReplyPayload => {
  return {
    kind: "overwrite",
  };
};

// create payload to rename a node
export const createRenamePayload = (newName: string): ReplyPayload => {
  return {
    kind: "rename",
    newName,
  };
};

// create object for reply
export const createReply = (taskId: string, payload: ReplyPayload): Reply => {
  return { taskId, reply: payload };
};
