// package: 
// file: service.proto

import * as jspb from "google-protobuf";

export class Request extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getCommand(): CommandMap[keyof CommandMap];
  setCommand(value: CommandMap[keyof CommandMap]): void;

  getPayload(): Uint8Array | string;
  getPayload_asU8(): Uint8Array;
  getPayload_asB64(): string;
  setPayload(value: Uint8Array | string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Request.AsObject;
  static toObject(includeInstance: boolean, msg: Request): Request.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Request, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Request;
  static deserializeBinaryFromReader(message: Request, reader: jspb.BinaryReader): Request;
}

export namespace Request {
  export type AsObject = {
    id: string,
    command: CommandMap[keyof CommandMap],
    payload: Uint8Array | string,
  }
}

export class ErrorDetail extends jspb.Message {
  getField(): string;
  setField(value: string): void;

  getErrorMessage(): string;
  setErrorMessage(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ErrorDetail.AsObject;
  static toObject(includeInstance: boolean, msg: ErrorDetail): ErrorDetail.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ErrorDetail, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ErrorDetail;
  static deserializeBinaryFromReader(message: ErrorDetail, reader: jspb.BinaryReader): ErrorDetail;
}

export namespace ErrorDetail {
  export type AsObject = {
    field: string,
    errorMessage: string,
  }
}

export class Error extends jspb.Message {
  getStatus(): number;
  setStatus(value: number): void;

  getErrorMessage(): string;
  setErrorMessage(value: string): void;

  clearDetailsList(): void;
  getDetailsList(): Array<ErrorDetail>;
  setDetailsList(value: Array<ErrorDetail>): void;
  addDetails(value?: ErrorDetail, index?: number): ErrorDetail;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Error.AsObject;
  static toObject(includeInstance: boolean, msg: Error): Error.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Error, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Error;
  static deserializeBinaryFromReader(message: Error, reader: jspb.BinaryReader): Error;
}

export namespace Error {
  export type AsObject = {
    status: number,
    errorMessage: string,
    detailsList: Array<ErrorDetail.AsObject>,
  }
}

export class Response extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getStatus(): StatusMap[keyof StatusMap];
  setStatus(value: StatusMap[keyof StatusMap]): void;

  getPayload(): Uint8Array | string;
  getPayload_asU8(): Uint8Array;
  getPayload_asB64(): string;
  setPayload(value: Uint8Array | string): void;

  hasError(): boolean;
  clearError(): void;
  getError(): Error | undefined;
  setError(value?: Error): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Response.AsObject;
  static toObject(includeInstance: boolean, msg: Response): Response.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Response, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Response;
  static deserializeBinaryFromReader(message: Response, reader: jspb.BinaryReader): Response;
}

export namespace Response {
  export type AsObject = {
    id: string,
    status: StatusMap[keyof StatusMap],
    payload: Uint8Array | string,
    error?: Error.AsObject,
  }
}

export interface CommandMap {
  UNKNOWN_COMMAND: 0;
  FILER_INITIALIZE: 1;
  FILER_RELOAD_ALL: 2;
  FILER_MOVE_LOCATION: 3;
  FILER_UPDATED: 4;
  FILER_COPY_INTERACTION: 5;
  FILER_MOVE_INTERACTION: 6;
  FILER_DELETE_INTERACTION: 7;
  KEYMAP_ADD_KEY_BINDING: 8;
  KEYMAP_REMOVE_KEY_BINDING: 9;
  KEYMAP_GET: 10;
  KEYMAP_RELOAD: 11;
  KEYMAP_UPDATED: 12;
  FILER_OPEN_FILE_ITEM: 13;
  CONFIGURATION_GET: 14;
  FILER_UP_DIRECTORY: 15;
  FILER_TOGGLE_MARK_OF_ITEM: 16;
  FILER_UPDATED_FILE_WINDOW: 17;
  COMPLETER_INITIALIZE: 18;
  COMPLETER_COMPLETE: 19;
  COMPLETER_NOTIFY_COMPLETED: 20;
  FILER_MOVE: 21;
  FILER_COPY: 22;
  FILER_DELETE: 23;
  CONFIGURATION_UPDATE: 24;
  CONFIGURATION_NOTIFY_UPDATED: 25;
  THEME_LIST: 26;
  THEME_ADD: 27;
  THEME_REMOVE: 28;
}

export const Command: CommandMap;

export interface StatusMap {
  UNKNOWN_STATUS: 0;
  SUCCESS: 1;
  INVALID_REQUEST_PAYLOAD: 2;
  COMMAND_FAILED: 3;
}

export const Status: StatusMap;

