// package: 
// file: jsonrpc.proto

import * as jspb from "google-protobuf";
import * as bookmark_pb from "./bookmark_pb";
import * as completion_pb from "./completion_pb";
import * as configuration_pb from "./configuration_pb";
import * as filer_pb from "./filer_pb";
import * as keymap_pb from "./keymap_pb";
import * as task_pb from "./task_pb";

export class Request extends jspb.Message {
  getJsonrpc(): string;
  setJsonrpc(value: string): void;

  getId(): string;
  setId(value: string): void;

  getMethod(): string;
  setMethod(value: string): void;

  hasKeymapgetrequest(): boolean;
  clearKeymapgetrequest(): void;
  getKeymapgetrequest(): keymap_pb.GetRequest | undefined;
  setKeymapgetrequest(value?: keymap_pb.GetRequest): void;

  hasKeymapreloadrequest(): boolean;
  clearKeymapreloadrequest(): void;
  getKeymapreloadrequest(): keymap_pb.ReloadRequest | undefined;
  setKeymapreloadrequest(value?: keymap_pb.ReloadRequest): void;

  hasKeymapstorerequest(): boolean;
  clearKeymapstorerequest(): void;
  getKeymapstorerequest(): keymap_pb.StoreRequest | undefined;
  setKeymapstorerequest(value?: keymap_pb.StoreRequest): void;

  hasFilermakerequest(): boolean;
  clearFilermakerequest(): void;
  getFilermakerequest(): filer_pb.MakeRequest | undefined;
  setFilermakerequest(value?: filer_pb.MakeRequest): void;

  hasFilergetrequest(): boolean;
  clearFilergetrequest(): void;
  getFilergetrequest(): filer_pb.GetRequest | undefined;
  setFilergetrequest(value?: filer_pb.GetRequest): void;

  hasFilermoveparentrequest(): boolean;
  clearFilermoveparentrequest(): void;
  getFilermoveparentrequest(): filer_pb.MoveParentRequest | undefined;
  setFilermoveparentrequest(value?: filer_pb.MoveParentRequest): void;

  hasFilerenterdirectoryrequest(): boolean;
  clearFilerenterdirectoryrequest(): void;
  getFilerenterdirectoryrequest(): filer_pb.EnterDirectoryRequest | undefined;
  setFilerenterdirectoryrequest(value?: filer_pb.EnterDirectoryRequest): void;

  hasFilertogglemarkrequest(): boolean;
  clearFilertogglemarkrequest(): void;
  getFilertogglemarkrequest(): filer_pb.ToggleMarkRequest | undefined;
  setFilertogglemarkrequest(value?: filer_pb.ToggleMarkRequest): void;

  hasFilermoverequest(): boolean;
  clearFilermoverequest(): void;
  getFilermoverequest(): filer_pb.MoveRequest | undefined;
  setFilermoverequest(value?: filer_pb.MoveRequest): void;

  hasFilerdeleterequest(): boolean;
  clearFilerdeleterequest(): void;
  getFilerdeleterequest(): filer_pb.DeleteRequest | undefined;
  setFilerdeleterequest(value?: filer_pb.DeleteRequest): void;

  hasFilercopyrequest(): boolean;
  clearFilercopyrequest(): void;
  getFilercopyrequest(): filer_pb.CopyRequest | undefined;
  setFilercopyrequest(value?: filer_pb.CopyRequest): void;

  hasFilerjumplocationrequest(): boolean;
  clearFilerjumplocationrequest(): void;
  getFilerjumplocationrequest(): filer_pb.JumpLocationRequest | undefined;
  setFilerjumplocationrequest(value?: filer_pb.JumpLocationRequest): void;

  hasConfigurationgetrequest(): boolean;
  clearConfigurationgetrequest(): void;
  getConfigurationgetrequest(): configuration_pb.GetRequest | undefined;
  setConfigurationgetrequest(value?: configuration_pb.GetRequest): void;

  hasConfigurationstorerequest(): boolean;
  clearConfigurationstorerequest(): void;
  getConfigurationstorerequest(): configuration_pb.StoreRequest | undefined;
  setConfigurationstorerequest(value?: configuration_pb.StoreRequest): void;

  hasCompletionsetuprequest(): boolean;
  clearCompletionsetuprequest(): void;
  getCompletionsetuprequest(): completion_pb.SetupRequest | undefined;
  setCompletionsetuprequest(value?: completion_pb.SetupRequest): void;

  hasCompletionreadrequest(): boolean;
  clearCompletionreadrequest(): void;
  getCompletionreadrequest(): completion_pb.ReadRequest | undefined;
  setCompletionreadrequest(value?: completion_pb.ReadRequest): void;

  hasBookmarklistallrequest(): boolean;
  clearBookmarklistallrequest(): void;
  getBookmarklistallrequest(): bookmark_pb.ListAllRequest | undefined;
  setBookmarklistallrequest(value?: bookmark_pb.ListAllRequest): void;

  hasBookmarkregisterrequest(): boolean;
  clearBookmarkregisterrequest(): void;
  getBookmarkregisterrequest(): bookmark_pb.RegisterRequest | undefined;
  setBookmarkregisterrequest(value?: bookmark_pb.RegisterRequest): void;

  hasBookmarkdeleterequest(): boolean;
  clearBookmarkdeleterequest(): void;
  getBookmarkdeleterequest(): bookmark_pb.DeleteRequest | undefined;
  setBookmarkdeleterequest(value?: bookmark_pb.DeleteRequest): void;

  hasTasksendreplyrequest(): boolean;
  clearTasksendreplyrequest(): void;
  getTasksendreplyrequest(): task_pb.SendReplyRequest | undefined;
  setTasksendreplyrequest(value?: task_pb.SendReplyRequest): void;

  hasTaskcancelrequest(): boolean;
  clearTaskcancelrequest(): void;
  getTaskcancelrequest(): task_pb.CancelRequest | undefined;
  setTaskcancelrequest(value?: task_pb.CancelRequest): void;

  getParamsCase(): Request.ParamsCase;
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
    jsonrpc: string,
    id: string,
    method: string,
    keymapgetrequest?: keymap_pb.GetRequest.AsObject,
    keymapreloadrequest?: keymap_pb.ReloadRequest.AsObject,
    keymapstorerequest?: keymap_pb.StoreRequest.AsObject,
    filermakerequest?: filer_pb.MakeRequest.AsObject,
    filergetrequest?: filer_pb.GetRequest.AsObject,
    filermoveparentrequest?: filer_pb.MoveParentRequest.AsObject,
    filerenterdirectoryrequest?: filer_pb.EnterDirectoryRequest.AsObject,
    filertogglemarkrequest?: filer_pb.ToggleMarkRequest.AsObject,
    filermoverequest?: filer_pb.MoveRequest.AsObject,
    filerdeleterequest?: filer_pb.DeleteRequest.AsObject,
    filercopyrequest?: filer_pb.CopyRequest.AsObject,
    filerjumplocationrequest?: filer_pb.JumpLocationRequest.AsObject,
    configurationgetrequest?: configuration_pb.GetRequest.AsObject,
    configurationstorerequest?: configuration_pb.StoreRequest.AsObject,
    completionsetuprequest?: completion_pb.SetupRequest.AsObject,
    completionreadrequest?: completion_pb.ReadRequest.AsObject,
    bookmarklistallrequest?: bookmark_pb.ListAllRequest.AsObject,
    bookmarkregisterrequest?: bookmark_pb.RegisterRequest.AsObject,
    bookmarkdeleterequest?: bookmark_pb.DeleteRequest.AsObject,
    tasksendreplyrequest?: task_pb.SendReplyRequest.AsObject,
    taskcancelrequest?: task_pb.CancelRequest.AsObject,
  }

  export enum ParamsCase {
    PARAMS_NOT_SET = 0,
    KEYMAPGETREQUEST = 4,
    KEYMAPRELOADREQUEST = 5,
    KEYMAPSTOREREQUEST = 6,
    FILERMAKEREQUEST = 7,
    FILERGETREQUEST = 8,
    FILERMOVEPARENTREQUEST = 9,
    FILERENTERDIRECTORYREQUEST = 10,
    FILERTOGGLEMARKREQUEST = 11,
    FILERMOVEREQUEST = 12,
    FILERDELETEREQUEST = 13,
    FILERCOPYREQUEST = 14,
    FILERJUMPLOCATIONREQUEST = 15,
    CONFIGURATIONGETREQUEST = 16,
    CONFIGURATIONSTOREREQUEST = 17,
    COMPLETIONSETUPREQUEST = 18,
    COMPLETIONREADREQUEST = 19,
    BOOKMARKLISTALLREQUEST = 20,
    BOOKMARKREGISTERREQUEST = 21,
    BOOKMARKDELETEREQUEST = 22,
    TASKSENDREPLYREQUEST = 23,
    TASKCANCELREQUEST = 24,
  }
}

export class RpcError extends jspb.Message {
  getCode(): number;
  setCode(value: number): void;

  getMessage(): string;
  setMessage(value: string): void;

  getData(): string;
  setData(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RpcError.AsObject;
  static toObject(includeInstance: boolean, msg: RpcError): RpcError.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RpcError, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RpcError;
  static deserializeBinaryFromReader(message: RpcError, reader: jspb.BinaryReader): RpcError;
}

export namespace RpcError {
  export type AsObject = {
    code: number,
    message: string,
    data: string,
  }
}

export class Result extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Result.AsObject;
  static toObject(includeInstance: boolean, msg: Result): Result.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Result, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Result;
  static deserializeBinaryFromReader(message: Result, reader: jspb.BinaryReader): Result;
}

export namespace Result {
  export type AsObject = {
  }
}

export class Response extends jspb.Message {
  getJsonrpc(): string;
  setJsonrpc(value: string): void;

  getId(): string;
  setId(value: string): void;

  hasError(): boolean;
  clearError(): void;
  getError(): RpcError | undefined;
  setError(value?: RpcError): void;

  hasEmpty(): boolean;
  clearEmpty(): void;
  getEmpty(): string;
  setEmpty(value: string): void;

  hasKeymapgetresponse(): boolean;
  clearKeymapgetresponse(): void;
  getKeymapgetresponse(): keymap_pb.GetResponse | undefined;
  setKeymapgetresponse(value?: keymap_pb.GetResponse): void;

  hasKeymapreloadresponse(): boolean;
  clearKeymapreloadresponse(): void;
  getKeymapreloadresponse(): keymap_pb.ReloadResponse | undefined;
  setKeymapreloadresponse(value?: keymap_pb.ReloadResponse): void;

  hasKeymapstoreresponse(): boolean;
  clearKeymapstoreresponse(): void;
  getKeymapstoreresponse(): keymap_pb.StoreResponse | undefined;
  setKeymapstoreresponse(value?: keymap_pb.StoreResponse): void;

  hasFilermakeresponse(): boolean;
  clearFilermakeresponse(): void;
  getFilermakeresponse(): filer_pb.MakeResponse | undefined;
  setFilermakeresponse(value?: filer_pb.MakeResponse): void;

  hasFilergetresponse(): boolean;
  clearFilergetresponse(): void;
  getFilergetresponse(): filer_pb.GetResponse | undefined;
  setFilergetresponse(value?: filer_pb.GetResponse): void;

  hasFilermoveparentresponse(): boolean;
  clearFilermoveparentresponse(): void;
  getFilermoveparentresponse(): filer_pb.MoveParentResponse | undefined;
  setFilermoveparentresponse(value?: filer_pb.MoveParentResponse): void;

  hasFilerenterdirectoryresponse(): boolean;
  clearFilerenterdirectoryresponse(): void;
  getFilerenterdirectoryresponse(): filer_pb.EnterDirectoryResponse | undefined;
  setFilerenterdirectoryresponse(value?: filer_pb.EnterDirectoryResponse): void;

  hasFilertogglemarkresponse(): boolean;
  clearFilertogglemarkresponse(): void;
  getFilertogglemarkresponse(): filer_pb.ToggleMarkResponse | undefined;
  setFilertogglemarkresponse(value?: filer_pb.ToggleMarkResponse): void;

  hasFilermoveresponse(): boolean;
  clearFilermoveresponse(): void;
  getFilermoveresponse(): filer_pb.MoveResponse | undefined;
  setFilermoveresponse(value?: filer_pb.MoveResponse): void;

  hasFilerdeleteresponse(): boolean;
  clearFilerdeleteresponse(): void;
  getFilerdeleteresponse(): filer_pb.DeleteResponse | undefined;
  setFilerdeleteresponse(value?: filer_pb.DeleteResponse): void;

  hasFilercopyresponse(): boolean;
  clearFilercopyresponse(): void;
  getFilercopyresponse(): filer_pb.CopyResponse | undefined;
  setFilercopyresponse(value?: filer_pb.CopyResponse): void;

  hasFilerjumplocationresponse(): boolean;
  clearFilerjumplocationresponse(): void;
  getFilerjumplocationresponse(): filer_pb.JumpLocationResponse | undefined;
  setFilerjumplocationresponse(value?: filer_pb.JumpLocationResponse): void;

  hasConfigurationgetresponse(): boolean;
  clearConfigurationgetresponse(): void;
  getConfigurationgetresponse(): configuration_pb.GetResponse | undefined;
  setConfigurationgetresponse(value?: configuration_pb.GetResponse): void;

  hasConfigurationstoreresponse(): boolean;
  clearConfigurationstoreresponse(): void;
  getConfigurationstoreresponse(): configuration_pb.StoreResponse | undefined;
  setConfigurationstoreresponse(value?: configuration_pb.StoreResponse): void;

  hasCompletionsetupresponse(): boolean;
  clearCompletionsetupresponse(): void;
  getCompletionsetupresponse(): completion_pb.SetupResponse | undefined;
  setCompletionsetupresponse(value?: completion_pb.SetupResponse): void;

  hasCompletionreadresponse(): boolean;
  clearCompletionreadresponse(): void;
  getCompletionreadresponse(): completion_pb.ReadResponse | undefined;
  setCompletionreadresponse(value?: completion_pb.ReadResponse): void;

  hasBookmarklistallresponse(): boolean;
  clearBookmarklistallresponse(): void;
  getBookmarklistallresponse(): bookmark_pb.ListAllResponse | undefined;
  setBookmarklistallresponse(value?: bookmark_pb.ListAllResponse): void;

  hasBookmarkregisterresponse(): boolean;
  clearBookmarkregisterresponse(): void;
  getBookmarkregisterresponse(): bookmark_pb.RegisterResponse | undefined;
  setBookmarkregisterresponse(value?: bookmark_pb.RegisterResponse): void;

  hasBookmarkdeleteresponse(): boolean;
  clearBookmarkdeleteresponse(): void;
  getBookmarkdeleteresponse(): bookmark_pb.DeleteResponse | undefined;
  setBookmarkdeleteresponse(value?: bookmark_pb.DeleteResponse): void;

  hasTasksendreplyresponse(): boolean;
  clearTasksendreplyresponse(): void;
  getTasksendreplyresponse(): task_pb.SendReplyResponse | undefined;
  setTasksendreplyresponse(value?: task_pb.SendReplyResponse): void;

  hasTaskcancelresponse(): boolean;
  clearTaskcancelresponse(): void;
  getTaskcancelresponse(): task_pb.CancelResponse | undefined;
  setTaskcancelresponse(value?: task_pb.CancelResponse): void;

  getResultCase(): Response.ResultCase;
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
    jsonrpc: string,
    id: string,
    error?: RpcError.AsObject,
    empty: string,
    keymapgetresponse?: keymap_pb.GetResponse.AsObject,
    keymapreloadresponse?: keymap_pb.ReloadResponse.AsObject,
    keymapstoreresponse?: keymap_pb.StoreResponse.AsObject,
    filermakeresponse?: filer_pb.MakeResponse.AsObject,
    filergetresponse?: filer_pb.GetResponse.AsObject,
    filermoveparentresponse?: filer_pb.MoveParentResponse.AsObject,
    filerenterdirectoryresponse?: filer_pb.EnterDirectoryResponse.AsObject,
    filertogglemarkresponse?: filer_pb.ToggleMarkResponse.AsObject,
    filermoveresponse?: filer_pb.MoveResponse.AsObject,
    filerdeleteresponse?: filer_pb.DeleteResponse.AsObject,
    filercopyresponse?: filer_pb.CopyResponse.AsObject,
    filerjumplocationresponse?: filer_pb.JumpLocationResponse.AsObject,
    configurationgetresponse?: configuration_pb.GetResponse.AsObject,
    configurationstoreresponse?: configuration_pb.StoreResponse.AsObject,
    completionsetupresponse?: completion_pb.SetupResponse.AsObject,
    completionreadresponse?: completion_pb.ReadResponse.AsObject,
    bookmarklistallresponse?: bookmark_pb.ListAllResponse.AsObject,
    bookmarkregisterresponse?: bookmark_pb.RegisterResponse.AsObject,
    bookmarkdeleteresponse?: bookmark_pb.DeleteResponse.AsObject,
    tasksendreplyresponse?: task_pb.SendReplyResponse.AsObject,
    taskcancelresponse?: task_pb.CancelResponse.AsObject,
  }

  export enum ResultCase {
    RESULT_NOT_SET = 0,
    EMPTY = 4,
    KEYMAPGETRESPONSE = 5,
    KEYMAPRELOADRESPONSE = 6,
    KEYMAPSTORERESPONSE = 7,
    FILERMAKERESPONSE = 8,
    FILERGETRESPONSE = 9,
    FILERMOVEPARENTRESPONSE = 10,
    FILERENTERDIRECTORYRESPONSE = 11,
    FILERTOGGLEMARKRESPONSE = 12,
    FILERMOVERESPONSE = 13,
    FILERDELETERESPONSE = 14,
    FILERCOPYRESPONSE = 15,
    FILERJUMPLOCATIONRESPONSE = 16,
    CONFIGURATIONGETRESPONSE = 17,
    CONFIGURATIONSTORERESPONSE = 18,
    COMPLETIONSETUPRESPONSE = 19,
    COMPLETIONREADRESPONSE = 20,
    BOOKMARKLISTALLRESPONSE = 21,
    BOOKMARKREGISTERRESPONSE = 22,
    BOOKMARKDELETERESPONSE = 23,
    TASKSENDREPLYRESPONSE = 24,
    TASKCANCELRESPONSE = 25,
  }
}

