type OverwritePayload = {
  readonly kind: "overwrite";
};

type RenamePayload = {
  readonly kind: "rename";
  readonly newName: string;
};

type PayloadObject = OverwritePayload | RenamePayload;
export type Payload = PayloadObject;

type ReplyObject = {
  readonly reply: Payload;
  readonly taskId: string;
};
export type Reply = ReplyObject;

// create payload to overwrite a node
export const createOverwritePayload = (): Payload => {
  return {
    kind: "overwrite",
  };
};

// create payload to rename a node
export const createRenamePayload = (newName: string): Payload => {
  return {
    kind: "rename",
    newName,
  };
};

// create object for reply
export const createReply = (taskId: string, payload: Payload): Reply => {
  return { taskId, reply: payload };
};
