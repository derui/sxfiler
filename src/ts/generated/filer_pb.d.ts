// package: 
// file: filer.proto

import * as jspb from "google-protobuf";
import * as types_pb from "./types_pb";

export class Capability extends jspb.Message {
  getWritable(): boolean;
  setWritable(value: boolean): void;

  getReadable(): boolean;
  setReadable(value: boolean): void;

  getExecutable(): boolean;
  setExecutable(value: boolean): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Capability.AsObject;
  static toObject(includeInstance: boolean, msg: Capability): Capability.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Capability, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Capability;
  static deserializeBinaryFromReader(message: Capability, reader: jspb.BinaryReader): Capability;
}

export namespace Capability {
  export type AsObject = {
    writable: boolean,
    readable: boolean,
    executable: boolean,
  }
}

export class Mode extends jspb.Message {
  hasOwner(): boolean;
  clearOwner(): void;
  getOwner(): Capability | undefined;
  setOwner(value?: Capability): void;

  hasGroup(): boolean;
  clearGroup(): void;
  getGroup(): Capability | undefined;
  setGroup(value?: Capability): void;

  hasOthers(): boolean;
  clearOthers(): void;
  getOthers(): Capability | undefined;
  setOthers(value?: Capability): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Mode.AsObject;
  static toObject(includeInstance: boolean, msg: Mode): Mode.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Mode, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Mode;
  static deserializeBinaryFromReader(message: Mode, reader: jspb.BinaryReader): Mode;
}

export namespace Mode {
  export type AsObject = {
    owner?: Capability.AsObject,
    group?: Capability.AsObject,
    others?: Capability.AsObject,
  }
}

export class FileStat extends jspb.Message {
  hasMode(): boolean;
  clearMode(): void;
  getMode(): Mode | undefined;
  setMode(value?: Mode): void;

  getUid(): number;
  setUid(value: number): void;

  getGid(): number;
  setGid(value: number): void;

  getAtime(): string;
  setAtime(value: string): void;

  getCtime(): string;
  setCtime(value: string): void;

  getMtime(): string;
  setMtime(value: string): void;

  getSize(): string;
  setSize(value: string): void;

  getLinkPath(): string;
  setLinkPath(value: string): void;

  getIsDirectory(): boolean;
  setIsDirectory(value: boolean): void;

  getIsFile(): boolean;
  setIsFile(value: boolean): void;

  getIsSymlink(): boolean;
  setIsSymlink(value: boolean): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileStat.AsObject;
  static toObject(includeInstance: boolean, msg: FileStat): FileStat.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: FileStat, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileStat;
  static deserializeBinaryFromReader(message: FileStat, reader: jspb.BinaryReader): FileStat;
}

export namespace FileStat {
  export type AsObject = {
    mode?: Mode.AsObject,
    uid: number,
    gid: number,
    atime: string,
    ctime: string,
    mtime: string,
    size: string,
    linkPath: string,
    isDirectory: boolean,
    isFile: boolean,
    isSymlink: boolean,
  }
}

export class FileItem extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getParent(): string;
  setParent(value: string): void;

  getName(): string;
  setName(value: string): void;

  getFullPath(): string;
  setFullPath(value: string): void;

  hasStat(): boolean;
  clearStat(): void;
  getStat(): FileStat | undefined;
  setStat(value?: FileStat): void;

  getHasLinkPath(): boolean;
  setHasLinkPath(value: boolean): void;

  getLinkPath(): string;
  setLinkPath(value: string): void;

  getMarked(): boolean;
  setMarked(value: boolean): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileItem.AsObject;
  static toObject(includeInstance: boolean, msg: FileItem): FileItem.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: FileItem, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileItem;
  static deserializeBinaryFromReader(message: FileItem, reader: jspb.BinaryReader): FileItem;
}

export namespace FileItem {
  export type AsObject = {
    id: string,
    parent: string,
    name: string,
    fullPath: string,
    stat?: FileStat.AsObject,
    hasLinkPath: boolean,
    linkPath: string,
    marked: boolean,
  }
}

export class FileList extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getLocation(): string;
  setLocation(value: string): void;

  clearItemsList(): void;
  getItemsList(): Array<FileItem>;
  setItemsList(value: Array<FileItem>): void;
  addItems(value?: FileItem, index?: number): FileItem;

  getSortOrder(): types_pb.SortTypeMap[keyof types_pb.SortTypeMap];
  setSortOrder(value: types_pb.SortTypeMap[keyof types_pb.SortTypeMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileList.AsObject;
  static toObject(includeInstance: boolean, msg: FileList): FileList.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: FileList, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileList;
  static deserializeBinaryFromReader(message: FileList, reader: jspb.BinaryReader): FileList;
}

export namespace FileList {
  export type AsObject = {
    id: string,
    location: string,
    itemsList: Array<FileItem.AsObject>,
    sortOrder: types_pb.SortTypeMap[keyof types_pb.SortTypeMap],
  }
}

export class LocationHistory extends jspb.Message {
  clearRecordsList(): void;
  getRecordsList(): Array<LocationRecord>;
  setRecordsList(value: Array<LocationRecord>): void;
  addRecords(value?: LocationRecord, index?: number): LocationRecord;

  getMaxRecordNumber(): number;
  setMaxRecordNumber(value: number): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): LocationHistory.AsObject;
  static toObject(includeInstance: boolean, msg: LocationHistory): LocationHistory.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: LocationHistory, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): LocationHistory;
  static deserializeBinaryFromReader(message: LocationHistory, reader: jspb.BinaryReader): LocationHistory;
}

export namespace LocationHistory {
  export type AsObject = {
    recordsList: Array<LocationRecord.AsObject>,
    maxRecordNumber: number,
  }
}

export class LocationRecord extends jspb.Message {
  getLocation(): string;
  setLocation(value: string): void;

  getTimestamp(): string;
  setTimestamp(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): LocationRecord.AsObject;
  static toObject(includeInstance: boolean, msg: LocationRecord): LocationRecord.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: LocationRecord, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): LocationRecord;
  static deserializeBinaryFromReader(message: LocationRecord, reader: jspb.BinaryReader): LocationRecord;
}

export namespace LocationRecord {
  export type AsObject = {
    location: string,
    timestamp: string,
  }
}

export class FileWindow extends jspb.Message {
  hasFileList(): boolean;
  clearFileList(): void;
  getFileList(): FileList | undefined;
  setFileList(value?: FileList): void;

  hasHistory(): boolean;
  clearHistory(): void;
  getHistory(): LocationHistory | undefined;
  setHistory(value?: LocationHistory): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileWindow.AsObject;
  static toObject(includeInstance: boolean, msg: FileWindow): FileWindow.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: FileWindow, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileWindow;
  static deserializeBinaryFromReader(message: FileWindow, reader: jspb.BinaryReader): FileWindow;
}

export namespace FileWindow {
  export type AsObject = {
    fileList?: FileList.AsObject,
    history?: LocationHistory.AsObject,
  }
}

export class Filer extends jspb.Message {
  hasLeftFileWindow(): boolean;
  clearLeftFileWindow(): void;
  getLeftFileWindow(): FileWindow | undefined;
  setLeftFileWindow(value?: FileWindow): void;

  hasRightFileWindow(): boolean;
  clearRightFileWindow(): void;
  getRightFileWindow(): FileWindow | undefined;
  setRightFileWindow(value?: FileWindow): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Filer.AsObject;
  static toObject(includeInstance: boolean, msg: Filer): Filer.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Filer, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Filer;
  static deserializeBinaryFromReader(message: Filer, reader: jspb.BinaryReader): Filer;
}

export namespace Filer {
  export type AsObject = {
    leftFileWindow?: FileWindow.AsObject,
    rightFileWindow?: FileWindow.AsObject,
  }
}

export class InitializeRequest extends jspb.Message {
  getLeftLocation(): string;
  setLeftLocation(value: string): void;

  getRightLocation(): string;
  setRightLocation(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): InitializeRequest.AsObject;
  static toObject(includeInstance: boolean, msg: InitializeRequest): InitializeRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: InitializeRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): InitializeRequest;
  static deserializeBinaryFromReader(message: InitializeRequest, reader: jspb.BinaryReader): InitializeRequest;
}

export namespace InitializeRequest {
  export type AsObject = {
    leftLocation: string,
    rightLocation: string,
  }
}

export class InitializeResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): InitializeResponse.AsObject;
  static toObject(includeInstance: boolean, msg: InitializeResponse): InitializeResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: InitializeResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): InitializeResponse;
  static deserializeBinaryFromReader(message: InitializeResponse, reader: jspb.BinaryReader): InitializeResponse;
}

export namespace InitializeResponse {
  export type AsObject = {
  }
}

export class ReloadAllRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ReloadAllRequest.AsObject;
  static toObject(includeInstance: boolean, msg: ReloadAllRequest): ReloadAllRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ReloadAllRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ReloadAllRequest;
  static deserializeBinaryFromReader(message: ReloadAllRequest, reader: jspb.BinaryReader): ReloadAllRequest;
}

export namespace ReloadAllRequest {
  export type AsObject = {
  }
}

export class ReloadAllResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ReloadAllResponse.AsObject;
  static toObject(includeInstance: boolean, msg: ReloadAllResponse): ReloadAllResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ReloadAllResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ReloadAllResponse;
  static deserializeBinaryFromReader(message: ReloadAllResponse, reader: jspb.BinaryReader): ReloadAllResponse;
}

export namespace ReloadAllResponse {
  export type AsObject = {
  }
}

export class MoveLocationRequest extends jspb.Message {
  getLocation(): string;
  setLocation(value: string): void;

  getSide(): SideMap[keyof SideMap];
  setSide(value: SideMap[keyof SideMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): MoveLocationRequest.AsObject;
  static toObject(includeInstance: boolean, msg: MoveLocationRequest): MoveLocationRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: MoveLocationRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): MoveLocationRequest;
  static deserializeBinaryFromReader(message: MoveLocationRequest, reader: jspb.BinaryReader): MoveLocationRequest;
}

export namespace MoveLocationRequest {
  export type AsObject = {
    location: string,
    side: SideMap[keyof SideMap],
  }
}

export class MoveLocationResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): MoveLocationResponse.AsObject;
  static toObject(includeInstance: boolean, msg: MoveLocationResponse): MoveLocationResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: MoveLocationResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): MoveLocationResponse;
  static deserializeBinaryFromReader(message: MoveLocationResponse, reader: jspb.BinaryReader): MoveLocationResponse;
}

export namespace MoveLocationResponse {
  export type AsObject = {
  }
}

export class UpDirectoryRequest extends jspb.Message {
  getSide(): SideMap[keyof SideMap];
  setSide(value: SideMap[keyof SideMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpDirectoryRequest.AsObject;
  static toObject(includeInstance: boolean, msg: UpDirectoryRequest): UpDirectoryRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpDirectoryRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpDirectoryRequest;
  static deserializeBinaryFromReader(message: UpDirectoryRequest, reader: jspb.BinaryReader): UpDirectoryRequest;
}

export namespace UpDirectoryRequest {
  export type AsObject = {
    side: SideMap[keyof SideMap],
  }
}

export class UpDirectoryResponse extends jspb.Message {
  getMoved(): boolean;
  setMoved(value: boolean): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpDirectoryResponse.AsObject;
  static toObject(includeInstance: boolean, msg: UpDirectoryResponse): UpDirectoryResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpDirectoryResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpDirectoryResponse;
  static deserializeBinaryFromReader(message: UpDirectoryResponse, reader: jspb.BinaryReader): UpDirectoryResponse;
}

export namespace UpDirectoryResponse {
  export type AsObject = {
    moved: boolean,
  }
}

export class OpenFileItemRequest extends jspb.Message {
  getFileItemId(): string;
  setFileItemId(value: string): void;

  getSide(): SideMap[keyof SideMap];
  setSide(value: SideMap[keyof SideMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): OpenFileItemRequest.AsObject;
  static toObject(includeInstance: boolean, msg: OpenFileItemRequest): OpenFileItemRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: OpenFileItemRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): OpenFileItemRequest;
  static deserializeBinaryFromReader(message: OpenFileItemRequest, reader: jspb.BinaryReader): OpenFileItemRequest;
}

export namespace OpenFileItemRequest {
  export type AsObject = {
    fileItemId: string,
    side: SideMap[keyof SideMap],
  }
}

export class OpenFileItemResponse extends jspb.Message {
  getResult(): OpenFileItemResponse.OpenResultMap[keyof OpenFileItemResponse.OpenResultMap];
  setResult(value: OpenFileItemResponse.OpenResultMap[keyof OpenFileItemResponse.OpenResultMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): OpenFileItemResponse.AsObject;
  static toObject(includeInstance: boolean, msg: OpenFileItemResponse): OpenFileItemResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: OpenFileItemResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): OpenFileItemResponse;
  static deserializeBinaryFromReader(message: OpenFileItemResponse, reader: jspb.BinaryReader): OpenFileItemResponse;
}

export namespace OpenFileItemResponse {
  export type AsObject = {
    result: OpenFileItemResponse.OpenResultMap[keyof OpenFileItemResponse.OpenResultMap],
  }

  export interface OpenResultMap {
    NOT_IMPLEMENTED: 0;
    DIRECTORY_OPENED: 1;
  }

  export const OpenResult: OpenResultMap;
}

export class ToggleMarkOfItemRequest extends jspb.Message {
  getItemId(): string;
  setItemId(value: string): void;

  getSide(): SideMap[keyof SideMap];
  setSide(value: SideMap[keyof SideMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ToggleMarkOfItemRequest.AsObject;
  static toObject(includeInstance: boolean, msg: ToggleMarkOfItemRequest): ToggleMarkOfItemRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ToggleMarkOfItemRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ToggleMarkOfItemRequest;
  static deserializeBinaryFromReader(message: ToggleMarkOfItemRequest, reader: jspb.BinaryReader): ToggleMarkOfItemRequest;
}

export namespace ToggleMarkOfItemRequest {
  export type AsObject = {
    itemId: string,
    side: SideMap[keyof SideMap],
  }
}

export class ToggleMarkOfItemResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ToggleMarkOfItemResponse.AsObject;
  static toObject(includeInstance: boolean, msg: ToggleMarkOfItemResponse): ToggleMarkOfItemResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ToggleMarkOfItemResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ToggleMarkOfItemResponse;
  static deserializeBinaryFromReader(message: ToggleMarkOfItemResponse, reader: jspb.BinaryReader): ToggleMarkOfItemResponse;
}

export namespace ToggleMarkOfItemResponse {
  export type AsObject = {
  }
}

export class CopyUserDecisionRequest extends jspb.Message {
  hasItem(): boolean;
  clearItem(): void;
  getItem(): FileItem | undefined;
  setItem(value?: FileItem): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CopyUserDecisionRequest.AsObject;
  static toObject(includeInstance: boolean, msg: CopyUserDecisionRequest): CopyUserDecisionRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: CopyUserDecisionRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CopyUserDecisionRequest;
  static deserializeBinaryFromReader(message: CopyUserDecisionRequest, reader: jspb.BinaryReader): CopyUserDecisionRequest;
}

export namespace CopyUserDecisionRequest {
  export type AsObject = {
    item?: FileItem.AsObject,
  }
}

export class CopyUserDecisionResponse extends jspb.Message {
  getAction(): ActionMap[keyof ActionMap];
  setAction(value: ActionMap[keyof ActionMap]): void;

  getNewName(): string;
  setNewName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CopyUserDecisionResponse.AsObject;
  static toObject(includeInstance: boolean, msg: CopyUserDecisionResponse): CopyUserDecisionResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: CopyUserDecisionResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CopyUserDecisionResponse;
  static deserializeBinaryFromReader(message: CopyUserDecisionResponse, reader: jspb.BinaryReader): CopyUserDecisionResponse;
}

export namespace CopyUserDecisionResponse {
  export type AsObject = {
    action: ActionMap[keyof ActionMap],
    newName: string,
  }
}

export class MoveUserDecisionRequest extends jspb.Message {
  hasItem(): boolean;
  clearItem(): void;
  getItem(): FileItem | undefined;
  setItem(value?: FileItem): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): MoveUserDecisionRequest.AsObject;
  static toObject(includeInstance: boolean, msg: MoveUserDecisionRequest): MoveUserDecisionRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: MoveUserDecisionRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): MoveUserDecisionRequest;
  static deserializeBinaryFromReader(message: MoveUserDecisionRequest, reader: jspb.BinaryReader): MoveUserDecisionRequest;
}

export namespace MoveUserDecisionRequest {
  export type AsObject = {
    item?: FileItem.AsObject,
  }
}

export class MoveUserDecisionResponse extends jspb.Message {
  getAction(): ActionMap[keyof ActionMap];
  setAction(value: ActionMap[keyof ActionMap]): void;

  getNewName(): string;
  setNewName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): MoveUserDecisionResponse.AsObject;
  static toObject(includeInstance: boolean, msg: MoveUserDecisionResponse): MoveUserDecisionResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: MoveUserDecisionResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): MoveUserDecisionResponse;
  static deserializeBinaryFromReader(message: MoveUserDecisionResponse, reader: jspb.BinaryReader): MoveUserDecisionResponse;
}

export namespace MoveUserDecisionResponse {
  export type AsObject = {
    action: ActionMap[keyof ActionMap],
    newName: string,
  }
}

export class DeleteUserDecisionRequest extends jspb.Message {
  hasItem(): boolean;
  clearItem(): void;
  getItem(): FileItem | undefined;
  setItem(value?: FileItem): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): DeleteUserDecisionRequest.AsObject;
  static toObject(includeInstance: boolean, msg: DeleteUserDecisionRequest): DeleteUserDecisionRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: DeleteUserDecisionRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): DeleteUserDecisionRequest;
  static deserializeBinaryFromReader(message: DeleteUserDecisionRequest, reader: jspb.BinaryReader): DeleteUserDecisionRequest;
}

export namespace DeleteUserDecisionRequest {
  export type AsObject = {
    item?: FileItem.AsObject,
  }
}

export class DeleteUserDecisionResponse extends jspb.Message {
  getConfirmed(): boolean;
  setConfirmed(value: boolean): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): DeleteUserDecisionResponse.AsObject;
  static toObject(includeInstance: boolean, msg: DeleteUserDecisionResponse): DeleteUserDecisionResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: DeleteUserDecisionResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): DeleteUserDecisionResponse;
  static deserializeBinaryFromReader(message: DeleteUserDecisionResponse, reader: jspb.BinaryReader): DeleteUserDecisionResponse;
}

export namespace DeleteUserDecisionResponse {
  export type AsObject = {
    confirmed: boolean,
  }
}

export class MoveRequest extends jspb.Message {
  getDirection(): DirectionMap[keyof DirectionMap];
  setDirection(value: DirectionMap[keyof DirectionMap]): void;

  getTarget(): TargetMap[keyof TargetMap];
  setTarget(value: TargetMap[keyof TargetMap]): void;

  getTargetId(): string;
  setTargetId(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): MoveRequest.AsObject;
  static toObject(includeInstance: boolean, msg: MoveRequest): MoveRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: MoveRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): MoveRequest;
  static deserializeBinaryFromReader(message: MoveRequest, reader: jspb.BinaryReader): MoveRequest;
}

export namespace MoveRequest {
  export type AsObject = {
    direction: DirectionMap[keyof DirectionMap],
    target: TargetMap[keyof TargetMap],
    targetId: string,
  }
}

export class MoveResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): MoveResponse.AsObject;
  static toObject(includeInstance: boolean, msg: MoveResponse): MoveResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: MoveResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): MoveResponse;
  static deserializeBinaryFromReader(message: MoveResponse, reader: jspb.BinaryReader): MoveResponse;
}

export namespace MoveResponse {
  export type AsObject = {
  }
}

export class UpdatedNotificationRequest extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpdatedNotificationRequest.AsObject;
  static toObject(includeInstance: boolean, msg: UpdatedNotificationRequest): UpdatedNotificationRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpdatedNotificationRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpdatedNotificationRequest;
  static deserializeBinaryFromReader(message: UpdatedNotificationRequest, reader: jspb.BinaryReader): UpdatedNotificationRequest;
}

export namespace UpdatedNotificationRequest {
  export type AsObject = {
    filer?: Filer.AsObject,
  }
}

export class UpdatedNotificationResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpdatedNotificationResponse.AsObject;
  static toObject(includeInstance: boolean, msg: UpdatedNotificationResponse): UpdatedNotificationResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpdatedNotificationResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpdatedNotificationResponse;
  static deserializeBinaryFromReader(message: UpdatedNotificationResponse, reader: jspb.BinaryReader): UpdatedNotificationResponse;
}

export namespace UpdatedNotificationResponse {
  export type AsObject = {
  }
}

export class UpdatedFileWindowNotificationRequest extends jspb.Message {
  hasFileWindow(): boolean;
  clearFileWindow(): void;
  getFileWindow(): FileWindow | undefined;
  setFileWindow(value?: FileWindow): void;

  getSide(): SideMap[keyof SideMap];
  setSide(value: SideMap[keyof SideMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpdatedFileWindowNotificationRequest.AsObject;
  static toObject(includeInstance: boolean, msg: UpdatedFileWindowNotificationRequest): UpdatedFileWindowNotificationRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpdatedFileWindowNotificationRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpdatedFileWindowNotificationRequest;
  static deserializeBinaryFromReader(message: UpdatedFileWindowNotificationRequest, reader: jspb.BinaryReader): UpdatedFileWindowNotificationRequest;
}

export namespace UpdatedFileWindowNotificationRequest {
  export type AsObject = {
    fileWindow?: FileWindow.AsObject,
    side: SideMap[keyof SideMap],
  }
}

export class UpdatedFileWindowNotificationResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpdatedFileWindowNotificationResponse.AsObject;
  static toObject(includeInstance: boolean, msg: UpdatedFileWindowNotificationResponse): UpdatedFileWindowNotificationResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpdatedFileWindowNotificationResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpdatedFileWindowNotificationResponse;
  static deserializeBinaryFromReader(message: UpdatedFileWindowNotificationResponse, reader: jspb.BinaryReader): UpdatedFileWindowNotificationResponse;
}

export namespace UpdatedFileWindowNotificationResponse {
  export type AsObject = {
  }
}

export interface SideMap {
  LEFT: 0;
  RIGHT: 1;
}

export const Side: SideMap;

export interface DirectionMap {
  LEFT_TO_RIGHT: 0;
  RIGHT_TO_LEFT: 1;
}

export const Direction: DirectionMap;

export interface TargetMap {
  MARKED: 0;
  ONE: 1;
}

export const Target: TargetMap;

export interface ActionMap {
  RENAME: 0;
  OVERWRITE: 1;
  CANCEL: 2;
}

export const Action: ActionMap;

