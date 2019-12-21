// package:
// file: keymap.proto

import * as jspb from "google-protobuf";

export class Keymap extends jspb.Message {
  clearBindingsList(): void;
  getBindingsList(): Array<Binding>;
  setBindingsList(value: Array<Binding>): void;
  addBindings(value?: Binding, index?: number): Binding;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Keymap.AsObject;
  static toObject(includeInstance: boolean, msg: Keymap): Keymap.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: Keymap, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Keymap;
  static deserializeBinaryFromReader(message: Keymap, reader: jspb.BinaryReader): Keymap;
}

export namespace Keymap {
  export type AsObject = {
    bindingsList: Array<Binding.AsObject>;
  };
}

export class Binding extends jspb.Message {
  getKey(): string;
  setKey(value: string): void;

  getAction(): string;
  setAction(value: string): void;

  clearContextsList(): void;
  getContextsList(): Array<string>;
  setContextsList(value: Array<string>): void;
  addContexts(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Binding.AsObject;
  static toObject(includeInstance: boolean, msg: Binding): Binding.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: Binding, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Binding;
  static deserializeBinaryFromReader(message: Binding, reader: jspb.BinaryReader): Binding;
}

export namespace Binding {
  export type AsObject = {
    key: string;
    action: string;
    contextsList: Array<string>;
  };
}

export class KeymapGetRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): KeymapGetRequest.AsObject;
  static toObject(includeInstance: boolean, msg: KeymapGetRequest): KeymapGetRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: KeymapGetRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): KeymapGetRequest;
  static deserializeBinaryFromReader(message: KeymapGetRequest, reader: jspb.BinaryReader): KeymapGetRequest;
}

export namespace KeymapGetRequest {
  export type AsObject = {};
}

export class KeymapGetResponse extends jspb.Message {
  hasKeymap(): boolean;
  clearKeymap(): void;
  getKeymap(): Keymap | undefined;
  setKeymap(value?: Keymap): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): KeymapGetResponse.AsObject;
  static toObject(includeInstance: boolean, msg: KeymapGetResponse): KeymapGetResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: KeymapGetResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): KeymapGetResponse;
  static deserializeBinaryFromReader(message: KeymapGetResponse, reader: jspb.BinaryReader): KeymapGetResponse;
}

export namespace KeymapGetResponse {
  export type AsObject = {
    keymap?: Keymap.AsObject;
  };
}

export class KeymapReloadRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): KeymapReloadRequest.AsObject;
  static toObject(includeInstance: boolean, msg: KeymapReloadRequest): KeymapReloadRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: KeymapReloadRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): KeymapReloadRequest;
  static deserializeBinaryFromReader(message: KeymapReloadRequest, reader: jspb.BinaryReader): KeymapReloadRequest;
}

export namespace KeymapReloadRequest {
  export type AsObject = {};
}

export class KeymapReloadResponse extends jspb.Message {
  hasKeymap(): boolean;
  clearKeymap(): void;
  getKeymap(): Keymap | undefined;
  setKeymap(value?: Keymap): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): KeymapReloadResponse.AsObject;
  static toObject(includeInstance: boolean, msg: KeymapReloadResponse): KeymapReloadResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: KeymapReloadResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): KeymapReloadResponse;
  static deserializeBinaryFromReader(message: KeymapReloadResponse, reader: jspb.BinaryReader): KeymapReloadResponse;
}

export namespace KeymapReloadResponse {
  export type AsObject = {
    keymap?: Keymap.AsObject;
  };
}

export class KeymapStoreRequest extends jspb.Message {
  hasKeymap(): boolean;
  clearKeymap(): void;
  getKeymap(): Keymap | undefined;
  setKeymap(value?: Keymap): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): KeymapStoreRequest.AsObject;
  static toObject(includeInstance: boolean, msg: KeymapStoreRequest): KeymapStoreRequest.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: KeymapStoreRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): KeymapStoreRequest;
  static deserializeBinaryFromReader(message: KeymapStoreRequest, reader: jspb.BinaryReader): KeymapStoreRequest;
}

export namespace KeymapStoreRequest {
  export type AsObject = {
    keymap?: Keymap.AsObject;
  };
}

export class KeymapStoreResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): KeymapStoreResponse.AsObject;
  static toObject(includeInstance: boolean, msg: KeymapStoreResponse): KeymapStoreResponse.AsObject;
  static extensions: { [key: number]: jspb.ExtensionFieldInfo<jspb.Message> };
  static extensionsBinary: { [key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message> };
  static serializeBinaryToWriter(message: KeymapStoreResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): KeymapStoreResponse;
  static deserializeBinaryFromReader(message: KeymapStoreResponse, reader: jspb.BinaryReader): KeymapStoreResponse;
}

export namespace KeymapStoreResponse {
  export type AsObject = {};
}
