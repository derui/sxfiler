// package:
// file: task.proto

import * as jspb from "google-protobuf";

export class TaskReply extends jspb.Message {
  getType(): ReplyTypeMap[keyof ReplyTypeMap];
  setType(value: ReplyTypeMap[keyof ReplyTypeMap]): void;

  hasOverwrite(): boolean;
  clearOverwrite(): void;
  getOverwrite(): boolean;
  setOverwrite(value: boolean): void;

  hasRename(): boolean;
  clearRename(): void;
  getRename(): TaskReply.Rename | undefined;
  setRename(value?: TaskReply.Rename): void;

  getTaskid(): string;
  setTaskid(value: string): void;

  getReplyCase(): TaskReply.ReplyCase;
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): TaskReply.AsObject;
  static toObject(includeInstance: boolean, msg: TaskReply): TaskReply.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: TaskReply, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): TaskReply;
  static deserializeBinaryFromReader(message: TaskReply, reader: jspb.BinaryReader): TaskReply;
}

export namespace TaskReply {
  export type AsObject = {
    type: ReplyTypeMap[keyof ReplyTypeMap];
    overwrite: boolean;
    rename?: TaskReply.Rename.AsObject;
    taskid: string;
  };

  export class Rename extends jspb.Message {
    getNewname(): string;
    setNewname(value: string): void;

    serializeBinary(): Uint8Array;
    toObject(includeInstance?: boolean): Rename.AsObject;
    static toObject(includeInstance: boolean, msg: Rename): Rename.AsObject;
    static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
    static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
    static serializeBinaryToWriter(message: Rename, writer: jspb.BinaryWriter): void;
    static deserializeBinary(bytes: Uint8Array): Rename;
    static deserializeBinaryFromReader(message: Rename, reader: jspb.BinaryReader): Rename;
  }

  export namespace Rename {
    export type AsObject = {
      newname: string;
    };
  }

  export enum ReplyCase {
    REPLY_NOT_SET = 0,
    OVERWRITE = 2,
    RENAME = 3,
  }
}

export class TaskSuggestion extends jspb.Message {
  clearSuggestionsList(): void;
  getSuggestionsList(): Array<ReplyTypeMap[keyof ReplyTypeMap]>;
  setSuggestionsList(value: Array<ReplyTypeMap[keyof ReplyTypeMap]>): void;
  addSuggestions(value: ReplyTypeMap[keyof ReplyTypeMap], index?: number): ReplyTypeMap[keyof ReplyTypeMap];

  getItemname(): string;
  setItemname(value: string): void;

  getTaskid(): string;
  setTaskid(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): TaskSuggestion.AsObject;
  static toObject(includeInstance: boolean, msg: TaskSuggestion): TaskSuggestion.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: TaskSuggestion, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): TaskSuggestion;
  static deserializeBinaryFromReader(message: TaskSuggestion, reader: jspb.BinaryReader): TaskSuggestion;
}

export namespace TaskSuggestion {
  export type AsObject = {
    suggestionsList: Array<ReplyTypeMap[keyof ReplyTypeMap]>;
    itemname: string;
    taskid: string;
  };
}

export class TaskSendReplyRequest extends jspb.Message {
  hasReply(): boolean;
  clearReply(): void;
  getReply(): TaskReply | undefined;
  setReply(value?: TaskReply): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): TaskSendReplyRequest.AsObject;
  static toObject(includeInstance: boolean, msg: TaskSendReplyRequest): TaskSendReplyRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: TaskSendReplyRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): TaskSendReplyRequest;
  static deserializeBinaryFromReader(message: TaskSendReplyRequest, reader: jspb.BinaryReader): TaskSendReplyRequest;
}

export namespace TaskSendReplyRequest {
  export type AsObject = {
    reply?: TaskReply.AsObject;
  };
}

export class TaskSendReplyResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): TaskSendReplyResponse.AsObject;
  static toObject(includeInstance: boolean, msg: TaskSendReplyResponse): TaskSendReplyResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: TaskSendReplyResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): TaskSendReplyResponse;
  static deserializeBinaryFromReader(message: TaskSendReplyResponse, reader: jspb.BinaryReader): TaskSendReplyResponse;
}

export namespace TaskSendReplyResponse {
  export type AsObject = {};
}

export class TaskCancelRequest extends jspb.Message {
  getTaskid(): string;
  setTaskid(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): TaskCancelRequest.AsObject;
  static toObject(includeInstance: boolean, msg: TaskCancelRequest): TaskCancelRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: TaskCancelRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): TaskCancelRequest;
  static deserializeBinaryFromReader(message: TaskCancelRequest, reader: jspb.BinaryReader): TaskCancelRequest;
}

export namespace TaskCancelRequest {
  export type AsObject = {
    taskid: string;
  };
}

export class TaskCancelResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): TaskCancelResponse.AsObject;
  static toObject(includeInstance: boolean, msg: TaskCancelResponse): TaskCancelResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: TaskCancelResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): TaskCancelResponse;
  static deserializeBinaryFromReader(message: TaskCancelResponse, reader: jspb.BinaryReader): TaskCancelResponse;
}

export namespace TaskCancelResponse {
  export type AsObject = {};
}

export interface ReplyTypeMap {
  OVERWRITE: 0;
  RENAME: 1;
}

export const ReplyType: ReplyTypeMap;
