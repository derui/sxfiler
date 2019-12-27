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
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: Capability, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Capability;
  static deserializeBinaryFromReader(message: Capability, reader: jspb.BinaryReader): Capability;
}

export namespace Capability {
  export type AsObject = {
    writable: boolean;
    readable: boolean;
    executable: boolean;
  };
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
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: Mode, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Mode;
  static deserializeBinaryFromReader(message: Mode, reader: jspb.BinaryReader): Mode;
}

export namespace Mode {
  export type AsObject = {
    owner?: Capability.AsObject;
    group?: Capability.AsObject;
    others?: Capability.AsObject;
  };
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

  getIsdirectory(): boolean;
  setIsdirectory(value: boolean): void;

  getIsfile(): boolean;
  setIsfile(value: boolean): void;

  getIssymlink(): boolean;
  setIssymlink(value: boolean): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileStat.AsObject;
  static toObject(includeInstance: boolean, msg: FileStat): FileStat.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FileStat, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileStat;
  static deserializeBinaryFromReader(message: FileStat, reader: jspb.BinaryReader): FileStat;
}

export namespace FileStat {
  export type AsObject = {
    mode?: Mode.AsObject;
    uid: number;
    gid: number;
    atime: string;
    ctime: string;
    mtime: string;
    size: string;
    isdirectory: boolean;
    isfile: boolean;
    issymlink: boolean;
  };
}

export class FileItem extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getParent(): string;
  setParent(value: string): void;

  getName(): string;
  setName(value: string): void;

  getFullpath(): string;
  setFullpath(value: string): void;

  hasStat(): boolean;
  clearStat(): void;
  getStat(): FileStat | undefined;
  setStat(value?: FileStat): void;

  getHaslinkpath(): boolean;
  setHaslinkpath(value: boolean): void;

  getLinkpath(): string;
  setLinkpath(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileItem.AsObject;
  static toObject(includeInstance: boolean, msg: FileItem): FileItem.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FileItem, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileItem;
  static deserializeBinaryFromReader(message: FileItem, reader: jspb.BinaryReader): FileItem;
}

export namespace FileItem {
  export type AsObject = {
    id: string;
    parent: string;
    name: string;
    fullpath: string;
    stat?: FileStat.AsObject;
    haslinkpath: boolean;
    linkpath: string;
  };
}

export class FileList extends jspb.Message {
  getLocation(): string;
  setLocation(value: string): void;

  clearItemsList(): void;
  getItemsList(): Array<FileItem>;
  setItemsList(value: Array<FileItem>): void;
  addItems(value?: FileItem, index?: number): FileItem;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FileList.AsObject;
  static toObject(includeInstance: boolean, msg: FileList): FileList.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FileList, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FileList;
  static deserializeBinaryFromReader(message: FileList, reader: jspb.BinaryReader): FileList;
}

export namespace FileList {
  export type AsObject = {
    location: string;
    itemsList: Array<FileItem.AsObject>;
  };
}

export class LocationHistory extends jspb.Message {
  clearRecordsList(): void;
  getRecordsList(): Array<LocationRecord>;
  setRecordsList(value: Array<LocationRecord>): void;
  addRecords(value?: LocationRecord, index?: number): LocationRecord;

  getMaxrecordnumber(): number;
  setMaxrecordnumber(value: number): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): LocationHistory.AsObject;
  static toObject(includeInstance: boolean, msg: LocationHistory): LocationHistory.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: LocationHistory, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): LocationHistory;
  static deserializeBinaryFromReader(message: LocationHistory, reader: jspb.BinaryReader): LocationHistory;
}

export namespace LocationHistory {
  export type AsObject = {
    recordsList: Array<LocationRecord.AsObject>;
    maxrecordnumber: number;
  };
}

export class LocationRecord extends jspb.Message {
  getLocation(): string;
  setLocation(value: string): void;

  getTimestamp(): string;
  setTimestamp(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): LocationRecord.AsObject;
  static toObject(includeInstance: boolean, msg: LocationRecord): LocationRecord.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: LocationRecord, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): LocationRecord;
  static deserializeBinaryFromReader(message: LocationRecord, reader: jspb.BinaryReader): LocationRecord;
}

export namespace LocationRecord {
  export type AsObject = {
    location: string;
    timestamp: string;
  };
}

export class Filer extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getName(): string;
  setName(value: string): void;

  hasFilelist(): boolean;
  clearFilelist(): void;
  getFilelist(): FileList | undefined;
  setFilelist(value?: FileList): void;

  hasHistory(): boolean;
  clearHistory(): void;
  getHistory(): LocationHistory | undefined;
  setHistory(value?: LocationHistory): void;

  clearMarkeditemsList(): void;
  getMarkeditemsList(): Array<string>;
  setMarkeditemsList(value: Array<string>): void;
  addMarkeditems(value: string, index?: number): string;

  getSortorder(): types_pb.SortTypeMap[keyof types_pb.SortTypeMap];
  setSortorder(value: types_pb.SortTypeMap[keyof types_pb.SortTypeMap]): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Filer.AsObject;
  static toObject(includeInstance: boolean, msg: Filer): Filer.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: Filer, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Filer;
  static deserializeBinaryFromReader(message: Filer, reader: jspb.BinaryReader): Filer;
}

export namespace Filer {
  export type AsObject = {
    id: string;
    name: string;
    filelist?: FileList.AsObject;
    history?: LocationHistory.AsObject;
    markeditemsList: Array<string>;
    sortorder: types_pb.SortTypeMap[keyof types_pb.SortTypeMap];
  };
}

export class FilerMakeRequest extends jspb.Message {
  getInitiallocation(): string;
  setInitiallocation(value: string): void;

  getName(): string;
  setName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerMakeRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerMakeRequest): FilerMakeRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerMakeRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerMakeRequest;
  static deserializeBinaryFromReader(message: FilerMakeRequest, reader: jspb.BinaryReader): FilerMakeRequest;
}

export namespace FilerMakeRequest {
  export type AsObject = {
    initiallocation: string;
    name: string;
  };
}

export class FilerMakeResponse extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerMakeResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerMakeResponse): FilerMakeResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerMakeResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerMakeResponse;
  static deserializeBinaryFromReader(message: FilerMakeResponse, reader: jspb.BinaryReader): FilerMakeResponse;
}

export namespace FilerMakeResponse {
  export type AsObject = {
    filer?: Filer.AsObject;
  };
}

export class FilerGetRequest extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerGetRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerGetRequest): FilerGetRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerGetRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerGetRequest;
  static deserializeBinaryFromReader(message: FilerGetRequest, reader: jspb.BinaryReader): FilerGetRequest;
}

export namespace FilerGetRequest {
  export type AsObject = {
    name: string;
  };
}

export class FilerGetResponse extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerGetResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerGetResponse): FilerGetResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerGetResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerGetResponse;
  static deserializeBinaryFromReader(message: FilerGetResponse, reader: jspb.BinaryReader): FilerGetResponse;
}

export namespace FilerGetResponse {
  export type AsObject = {
    filer?: Filer.AsObject;
  };
}

export class FilerMoveParentRequest extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerMoveParentRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerMoveParentRequest): FilerMoveParentRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerMoveParentRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerMoveParentRequest;
  static deserializeBinaryFromReader(
    message: FilerMoveParentRequest,
    reader: jspb.BinaryReader
  ): FilerMoveParentRequest;
}

export namespace FilerMoveParentRequest {
  export type AsObject = {
    name: string;
  };
}

export class FilerMoveParentResponse extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerMoveParentResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerMoveParentResponse): FilerMoveParentResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerMoveParentResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerMoveParentResponse;
  static deserializeBinaryFromReader(
    message: FilerMoveParentResponse,
    reader: jspb.BinaryReader
  ): FilerMoveParentResponse;
}

export namespace FilerMoveParentResponse {
  export type AsObject = {
    filer?: Filer.AsObject;
  };
}

export class FilerEnterDirectoryRequest extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  getItemid(): string;
  setItemid(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerEnterDirectoryRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerEnterDirectoryRequest): FilerEnterDirectoryRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerEnterDirectoryRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerEnterDirectoryRequest;
  static deserializeBinaryFromReader(
    message: FilerEnterDirectoryRequest,
    reader: jspb.BinaryReader
  ): FilerEnterDirectoryRequest;
}

export namespace FilerEnterDirectoryRequest {
  export type AsObject = {
    name: string;
    itemid: string;
  };
}

export class FilerEnterDirectoryResponse extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerEnterDirectoryResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerEnterDirectoryResponse): FilerEnterDirectoryResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerEnterDirectoryResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerEnterDirectoryResponse;
  static deserializeBinaryFromReader(
    message: FilerEnterDirectoryResponse,
    reader: jspb.BinaryReader
  ): FilerEnterDirectoryResponse;
}

export namespace FilerEnterDirectoryResponse {
  export type AsObject = {
    filer?: Filer.AsObject;
  };
}

export class FilerToggleMarkRequest extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  clearItemidsList(): void;
  getItemidsList(): Array<string>;
  setItemidsList(value: Array<string>): void;
  addItemids(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerToggleMarkRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerToggleMarkRequest): FilerToggleMarkRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerToggleMarkRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerToggleMarkRequest;
  static deserializeBinaryFromReader(
    message: FilerToggleMarkRequest,
    reader: jspb.BinaryReader
  ): FilerToggleMarkRequest;
}

export namespace FilerToggleMarkRequest {
  export type AsObject = {
    name: string;
    itemidsList: Array<string>;
  };
}

export class FilerToggleMarkResponse extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerToggleMarkResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerToggleMarkResponse): FilerToggleMarkResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerToggleMarkResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerToggleMarkResponse;
  static deserializeBinaryFromReader(
    message: FilerToggleMarkResponse,
    reader: jspb.BinaryReader
  ): FilerToggleMarkResponse;
}

export namespace FilerToggleMarkResponse {
  export type AsObject = {
    filer?: Filer.AsObject;
  };
}

export class FilerMoveRequest extends jspb.Message {
  getSource(): string;
  setSource(value: string): void;

  getDest(): string;
  setDest(value: string): void;

  clearItemidsList(): void;
  getItemidsList(): Array<string>;
  setItemidsList(value: Array<string>): void;
  addItemids(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerMoveRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerMoveRequest): FilerMoveRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerMoveRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerMoveRequest;
  static deserializeBinaryFromReader(message: FilerMoveRequest, reader: jspb.BinaryReader): FilerMoveRequest;
}

export namespace FilerMoveRequest {
  export type AsObject = {
    source: string;
    dest: string;
    itemidsList: Array<string>;
  };
}

export class FilerMoveResponse extends jspb.Message {
  getTaskid(): string;
  setTaskid(value: string): void;

  getTaskname(): string;
  setTaskname(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerMoveResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerMoveResponse): FilerMoveResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerMoveResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerMoveResponse;
  static deserializeBinaryFromReader(message: FilerMoveResponse, reader: jspb.BinaryReader): FilerMoveResponse;
}

export namespace FilerMoveResponse {
  export type AsObject = {
    taskid: string;
    taskname: string;
  };
}

export class FilerDeleteRequest extends jspb.Message {
  getSource(): string;
  setSource(value: string): void;

  clearItemidsList(): void;
  getItemidsList(): Array<string>;
  setItemidsList(value: Array<string>): void;
  addItemids(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerDeleteRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerDeleteRequest): FilerDeleteRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerDeleteRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerDeleteRequest;
  static deserializeBinaryFromReader(message: FilerDeleteRequest, reader: jspb.BinaryReader): FilerDeleteRequest;
}

export namespace FilerDeleteRequest {
  export type AsObject = {
    source: string;
    itemidsList: Array<string>;
  };
}

export class FilerDeleteResponse extends jspb.Message {
  getTaskid(): string;
  setTaskid(value: string): void;

  getTaskname(): string;
  setTaskname(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerDeleteResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerDeleteResponse): FilerDeleteResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerDeleteResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerDeleteResponse;
  static deserializeBinaryFromReader(message: FilerDeleteResponse, reader: jspb.BinaryReader): FilerDeleteResponse;
}

export namespace FilerDeleteResponse {
  export type AsObject = {
    taskid: string;
    taskname: string;
  };
}

export class FilerCopyRequest extends jspb.Message {
  getSource(): string;
  setSource(value: string): void;

  getDest(): string;
  setDest(value: string): void;

  clearItemidsList(): void;
  getItemidsList(): Array<string>;
  setItemidsList(value: Array<string>): void;
  addItemids(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerCopyRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerCopyRequest): FilerCopyRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerCopyRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerCopyRequest;
  static deserializeBinaryFromReader(message: FilerCopyRequest, reader: jspb.BinaryReader): FilerCopyRequest;
}

export namespace FilerCopyRequest {
  export type AsObject = {
    source: string;
    dest: string;
    itemidsList: Array<string>;
  };
}

export class FilerCopyResponse extends jspb.Message {
  getTaskid(): string;
  setTaskid(value: string): void;

  getTaskname(): string;
  setTaskname(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerCopyResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerCopyResponse): FilerCopyResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerCopyResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerCopyResponse;
  static deserializeBinaryFromReader(message: FilerCopyResponse, reader: jspb.BinaryReader): FilerCopyResponse;
}

export namespace FilerCopyResponse {
  export type AsObject = {
    taskid: string;
    taskname: string;
  };
}

export class FilerJumpLocationRequest extends jspb.Message {
  getLocation(): string;
  setLocation(value: string): void;

  getName(): string;
  setName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerJumpLocationRequest.AsObject;
  static toObject(includeInstance: boolean, msg: FilerJumpLocationRequest): FilerJumpLocationRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerJumpLocationRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerJumpLocationRequest;
  static deserializeBinaryFromReader(
    message: FilerJumpLocationRequest,
    reader: jspb.BinaryReader
  ): FilerJumpLocationRequest;
}

export namespace FilerJumpLocationRequest {
  export type AsObject = {
    location: string;
    name: string;
  };
}

export class FilerJumpLocationResponse extends jspb.Message {
  hasFiler(): boolean;
  clearFiler(): void;
  getFiler(): Filer | undefined;
  setFiler(value?: Filer): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): FilerJumpLocationResponse.AsObject;
  static toObject(includeInstance: boolean, msg: FilerJumpLocationResponse): FilerJumpLocationResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: FilerJumpLocationResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): FilerJumpLocationResponse;
  static deserializeBinaryFromReader(
    message: FilerJumpLocationResponse,
    reader: jspb.BinaryReader
  ): FilerJumpLocationResponse;
}

export namespace FilerJumpLocationResponse {
  export type AsObject = {
    filer?: Filer.AsObject;
  };
}
