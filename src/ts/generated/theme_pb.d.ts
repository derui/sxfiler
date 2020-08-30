// package: theme
// file: theme.proto

import * as jspb from "google-protobuf";

export class ColorPair extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  getHexColor(): string;
  setHexColor(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ColorPair.AsObject;
  static toObject(includeInstance: boolean, msg: ColorPair): ColorPair.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ColorPair, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ColorPair;
  static deserializeBinaryFromReader(message: ColorPair, reader: jspb.BinaryReader): ColorPair;
}

export namespace ColorPair {
  export type AsObject = {
    name: string,
    hexColor: string,
  }
}

export class ColorTheme extends jspb.Message {
  clearColorPairsList(): void;
  getColorPairsList(): Array<ColorPair>;
  setColorPairsList(value: Array<ColorPair>): void;
  addColorPairs(value?: ColorPair, index?: number): ColorPair;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ColorTheme.AsObject;
  static toObject(includeInstance: boolean, msg: ColorTheme): ColorTheme.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ColorTheme, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ColorTheme;
  static deserializeBinaryFromReader(message: ColorTheme, reader: jspb.BinaryReader): ColorTheme;
}

export namespace ColorTheme {
  export type AsObject = {
    colorPairsList: Array<ColorPair.AsObject>,
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
  hasTheme(): boolean;
  clearTheme(): void;
  getTheme(): ColorTheme | undefined;
  setTheme(value?: ColorTheme): void;

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
    theme?: ColorTheme.AsObject,
  }
}

export class UpdateRequest extends jspb.Message {
  clearColorPairsList(): void;
  getColorPairsList(): Array<ColorPair>;
  setColorPairsList(value: Array<ColorPair>): void;
  addColorPairs(value?: ColorPair, index?: number): ColorPair;

  getBaseTheme(): string;
  setBaseTheme(value: string): void;

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
    colorPairsList: Array<ColorPair.AsObject>,
    baseTheme: string,
  }
}

export class UpdateResponse extends jspb.Message {
  hasTheme(): boolean;
  clearTheme(): void;
  getTheme(): ColorTheme | undefined;
  setTheme(value?: ColorTheme): void;

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
    theme?: ColorTheme.AsObject,
  }
}

