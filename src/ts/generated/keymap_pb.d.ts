// package: keymap
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
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Keymap, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Keymap;
  static deserializeBinaryFromReader(message: Keymap, reader: jspb.BinaryReader): Keymap;
}

export namespace Keymap {
  export type AsObject = {
    bindingsList: Array<Binding.AsObject>,
  }
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
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Binding, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Binding;
  static deserializeBinaryFromReader(message: Binding, reader: jspb.BinaryReader): Binding;
}

export namespace Binding {
  export type AsObject = {
    key: string,
    action: string,
    contextsList: Array<string>,
  }
}

export class GetRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): GetRequest.AsObject;
  static toObject(includeInstance: boolean, msg: GetRequest): GetRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: GetRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): GetRequest;
  static deserializeBinaryFromReader(message: GetRequest, reader: jspb.BinaryReader): GetRequest;
}

export namespace GetRequest {
  export type AsObject = {
  }
}

export class GetResponse extends jspb.Message {
  hasKeymap(): boolean;
  clearKeymap(): void;
  getKeymap(): Keymap | undefined;
  setKeymap(value?: Keymap): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): GetResponse.AsObject;
  static toObject(includeInstance: boolean, msg: GetResponse): GetResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: GetResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): GetResponse;
  static deserializeBinaryFromReader(message: GetResponse, reader: jspb.BinaryReader): GetResponse;
}

export namespace GetResponse {
  export type AsObject = {
    keymap?: Keymap.AsObject,
  }
}

export class ReloadRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ReloadRequest.AsObject;
  static toObject(includeInstance: boolean, msg: ReloadRequest): ReloadRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ReloadRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ReloadRequest;
  static deserializeBinaryFromReader(message: ReloadRequest, reader: jspb.BinaryReader): ReloadRequest;
}

export namespace ReloadRequest {
  export type AsObject = {
  }
}

export class ReloadResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ReloadResponse.AsObject;
  static toObject(includeInstance: boolean, msg: ReloadResponse): ReloadResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ReloadResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ReloadResponse;
  static deserializeBinaryFromReader(message: ReloadResponse, reader: jspb.BinaryReader): ReloadResponse;
}

export namespace ReloadResponse {
  export type AsObject = {
  }
}

export class AddKeyBindingRequest extends jspb.Message {
  getKey(): string;
  setKey(value: string): void;

  getAction(): string;
  setAction(value: string): void;

  clearContextsList(): void;
  getContextsList(): Array<string>;
  setContextsList(value: Array<string>): void;
  addContexts(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): AddKeyBindingRequest.AsObject;
  static toObject(includeInstance: boolean, msg: AddKeyBindingRequest): AddKeyBindingRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: AddKeyBindingRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): AddKeyBindingRequest;
  static deserializeBinaryFromReader(message: AddKeyBindingRequest, reader: jspb.BinaryReader): AddKeyBindingRequest;
}

export namespace AddKeyBindingRequest {
  export type AsObject = {
    key: string,
    action: string,
    contextsList: Array<string>,
  }
}

export class AddKeyBindingResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): AddKeyBindingResponse.AsObject;
  static toObject(includeInstance: boolean, msg: AddKeyBindingResponse): AddKeyBindingResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: AddKeyBindingResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): AddKeyBindingResponse;
  static deserializeBinaryFromReader(message: AddKeyBindingResponse, reader: jspb.BinaryReader): AddKeyBindingResponse;
}

export namespace AddKeyBindingResponse {
  export type AsObject = {
  }
}

export class RemoveKeyBindingRequest extends jspb.Message {
  getKey(): string;
  setKey(value: string): void;

  clearContextsList(): void;
  getContextsList(): Array<string>;
  setContextsList(value: Array<string>): void;
  addContexts(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RemoveKeyBindingRequest.AsObject;
  static toObject(includeInstance: boolean, msg: RemoveKeyBindingRequest): RemoveKeyBindingRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RemoveKeyBindingRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RemoveKeyBindingRequest;
  static deserializeBinaryFromReader(message: RemoveKeyBindingRequest, reader: jspb.BinaryReader): RemoveKeyBindingRequest;
}

export namespace RemoveKeyBindingRequest {
  export type AsObject = {
    key: string,
    contextsList: Array<string>,
  }
}

export class RemoveKeyBindingResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RemoveKeyBindingResponse.AsObject;
  static toObject(includeInstance: boolean, msg: RemoveKeyBindingResponse): RemoveKeyBindingResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RemoveKeyBindingResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RemoveKeyBindingResponse;
  static deserializeBinaryFromReader(message: RemoveKeyBindingResponse, reader: jspb.BinaryReader): RemoveKeyBindingResponse;
}

export namespace RemoveKeyBindingResponse {
  export type AsObject = {
  }
}

