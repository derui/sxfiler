// package: 
// file: bookmark.proto

import * as jspb from "google-protobuf";

export class Bookmark extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  getPath(): string;
  setPath(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Bookmark.AsObject;
  static toObject(includeInstance: boolean, msg: Bookmark): Bookmark.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Bookmark, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Bookmark;
  static deserializeBinaryFromReader(message: Bookmark, reader: jspb.BinaryReader): Bookmark;
}

export namespace Bookmark {
  export type AsObject = {
    name: string,
    path: string,
  }
}

export class BookmarkList extends jspb.Message {
  clearItemsList(): void;
  getItemsList(): Array<Bookmark>;
  setItemsList(value: Array<Bookmark>): void;
  addItems(value?: Bookmark, index?: number): Bookmark;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): BookmarkList.AsObject;
  static toObject(includeInstance: boolean, msg: BookmarkList): BookmarkList.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: BookmarkList, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): BookmarkList;
  static deserializeBinaryFromReader(message: BookmarkList, reader: jspb.BinaryReader): BookmarkList;
}

export namespace BookmarkList {
  export type AsObject = {
    itemsList: Array<Bookmark.AsObject>,
  }
}

export class ListAllRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ListAllRequest.AsObject;
  static toObject(includeInstance: boolean, msg: ListAllRequest): ListAllRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ListAllRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ListAllRequest;
  static deserializeBinaryFromReader(message: ListAllRequest, reader: jspb.BinaryReader): ListAllRequest;
}

export namespace ListAllRequest {
  export type AsObject = {
  }
}

export class ListAllResponse extends jspb.Message {
  hasBookmarks(): boolean;
  clearBookmarks(): void;
  getBookmarks(): BookmarkList | undefined;
  setBookmarks(value?: BookmarkList): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ListAllResponse.AsObject;
  static toObject(includeInstance: boolean, msg: ListAllResponse): ListAllResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ListAllResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ListAllResponse;
  static deserializeBinaryFromReader(message: ListAllResponse, reader: jspb.BinaryReader): ListAllResponse;
}

export namespace ListAllResponse {
  export type AsObject = {
    bookmarks?: BookmarkList.AsObject,
  }
}

export class RegisterRequest extends jspb.Message {
  getPath(): string;
  setPath(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RegisterRequest.AsObject;
  static toObject(includeInstance: boolean, msg: RegisterRequest): RegisterRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RegisterRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RegisterRequest;
  static deserializeBinaryFromReader(message: RegisterRequest, reader: jspb.BinaryReader): RegisterRequest;
}

export namespace RegisterRequest {
  export type AsObject = {
    path: string,
  }
}

export class RegisterResponse extends jspb.Message {
  hasBookmark(): boolean;
  clearBookmark(): void;
  getBookmark(): Bookmark | undefined;
  setBookmark(value?: Bookmark): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RegisterResponse.AsObject;
  static toObject(includeInstance: boolean, msg: RegisterResponse): RegisterResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RegisterResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RegisterResponse;
  static deserializeBinaryFromReader(message: RegisterResponse, reader: jspb.BinaryReader): RegisterResponse;
}

export namespace RegisterResponse {
  export type AsObject = {
    bookmark?: Bookmark.AsObject,
  }
}

export class DeleteRequest extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): DeleteRequest.AsObject;
  static toObject(includeInstance: boolean, msg: DeleteRequest): DeleteRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: DeleteRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): DeleteRequest;
  static deserializeBinaryFromReader(message: DeleteRequest, reader: jspb.BinaryReader): DeleteRequest;
}

export namespace DeleteRequest {
  export type AsObject = {
    id: string,
  }
}

export class DeleteResponse extends jspb.Message {
  hasDeletedBookmarks(): boolean;
  clearDeletedBookmarks(): void;
  getDeletedBookmarks(): Bookmark | undefined;
  setDeletedBookmarks(value?: Bookmark): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): DeleteResponse.AsObject;
  static toObject(includeInstance: boolean, msg: DeleteResponse): DeleteResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: DeleteResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): DeleteResponse;
  static deserializeBinaryFromReader(message: DeleteResponse, reader: jspb.BinaryReader): DeleteResponse;
}

export namespace DeleteResponse {
  export type AsObject = {
    deletedBookmarks?: Bookmark.AsObject,
  }
}

