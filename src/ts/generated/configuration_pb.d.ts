// package: configuration
// file: configuration.proto

import * as jspb from "google-protobuf";
import * as types_pb from "./types_pb";

export class Configuration extends jspb.Message {
  clearKeyList(): void;
  getKeyList(): Array<string>;
  setKeyList(value: Array<string>): void;
  addKey(value: string, index?: number): string;

  getJsonValue(): string;
  setJsonValue(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Configuration.AsObject;
  static toObject(includeInstance: boolean, msg: Configuration): Configuration.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Configuration, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Configuration;
  static deserializeBinaryFromReader(message: Configuration, reader: jspb.BinaryReader): Configuration;
}

export namespace Configuration {
  export type AsObject = {
    keyList: Array<string>,
    jsonValue: string,
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
  clearConfigurationsList(): void;
  getConfigurationsList(): Array<Configuration>;
  setConfigurationsList(value: Array<Configuration>): void;
  addConfigurations(value?: Configuration, index?: number): Configuration;

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
    configurationsList: Array<Configuration.AsObject>,
  }
}

export class UpdateRequest extends jspb.Message {
  clearKeyList(): void;
  getKeyList(): Array<string>;
  setKeyList(value: Array<string>): void;
  addKey(value: string, index?: number): string;

  getJsonValue(): string;
  setJsonValue(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpdateRequest.AsObject;
  static toObject(includeInstance: boolean, msg: UpdateRequest): UpdateRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpdateRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpdateRequest;
  static deserializeBinaryFromReader(message: UpdateRequest, reader: jspb.BinaryReader): UpdateRequest;
}

export namespace UpdateRequest {
  export type AsObject = {
    keyList: Array<string>,
    jsonValue: string,
  }
}

export class UpdateResponse extends jspb.Message {
  clearKeyList(): void;
  getKeyList(): Array<string>;
  setKeyList(value: Array<string>): void;
  addKey(value: string, index?: number): string;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): UpdateResponse.AsObject;
  static toObject(includeInstance: boolean, msg: UpdateResponse): UpdateResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: UpdateResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): UpdateResponse;
  static deserializeBinaryFromReader(message: UpdateResponse, reader: jspb.BinaryReader): UpdateResponse;
}

export namespace UpdateResponse {
  export type AsObject = {
    keyList: Array<string>,
  }
}

export class UpdatedNotificationRequest extends jspb.Message {
  clearConfigurationsList(): void;
  getConfigurationsList(): Array<Configuration>;
  setConfigurationsList(value: Array<Configuration>): void;
  addConfigurations(value?: Configuration, index?: number): Configuration;

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
    configurationsList: Array<Configuration.AsObject>,
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

