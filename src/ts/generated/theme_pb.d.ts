// package: theme
// file: theme.proto

import * as jspb from "google-protobuf";

export class ColorCode extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  getHexColor(): string;
  setHexColor(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ColorCode.AsObject;
  static toObject(includeInstance: boolean, msg: ColorCode): ColorCode.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ColorCode, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ColorCode;
  static deserializeBinaryFromReader(message: ColorCode, reader: jspb.BinaryReader): ColorCode;
}

export namespace ColorCode {
  export type AsObject = {
    name: string,
    hexColor: string,
  }
}

export class Theme extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  getDescription(): string;
  setDescription(value: string): void;

  clearColorCodesList(): void;
  getColorCodesList(): Array<ColorCode>;
  setColorCodesList(value: Array<ColorCode>): void;
  addColorCodes(value?: ColorCode, index?: number): ColorCode;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Theme.AsObject;
  static toObject(includeInstance: boolean, msg: Theme): Theme.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Theme, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Theme;
  static deserializeBinaryFromReader(message: Theme, reader: jspb.BinaryReader): Theme;
}

export namespace Theme {
  export type AsObject = {
    name: string,
    description: string,
    colorCodesList: Array<ColorCode.AsObject>,
  }
}

export class ListRequest extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ListRequest.AsObject;
  static toObject(includeInstance: boolean, msg: ListRequest): ListRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ListRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ListRequest;
  static deserializeBinaryFromReader(message: ListRequest, reader: jspb.BinaryReader): ListRequest;
}

export namespace ListRequest {
  export type AsObject = {
  }
}

export class ListResponse extends jspb.Message {
  clearThemesList(): void;
  getThemesList(): Array<Theme>;
  setThemesList(value: Array<Theme>): void;
  addThemes(value?: Theme, index?: number): Theme;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): ListResponse.AsObject;
  static toObject(includeInstance: boolean, msg: ListResponse): ListResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: ListResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): ListResponse;
  static deserializeBinaryFromReader(message: ListResponse, reader: jspb.BinaryReader): ListResponse;
}

export namespace ListResponse {
  export type AsObject = {
    themesList: Array<Theme.AsObject>,
  }
}

export class RemoveRequest extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RemoveRequest.AsObject;
  static toObject(includeInstance: boolean, msg: RemoveRequest): RemoveRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RemoveRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RemoveRequest;
  static deserializeBinaryFromReader(message: RemoveRequest, reader: jspb.BinaryReader): RemoveRequest;
}

export namespace RemoveRequest {
  export type AsObject = {
    name: string,
  }
}

export class RemoveResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): RemoveResponse.AsObject;
  static toObject(includeInstance: boolean, msg: RemoveResponse): RemoveResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: RemoveResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): RemoveResponse;
  static deserializeBinaryFromReader(message: RemoveResponse, reader: jspb.BinaryReader): RemoveResponse;
}

export namespace RemoveResponse {
  export type AsObject = {
  }
}

export class AddRequest extends jspb.Message {
  getName(): string;
  setName(value: string): void;

  getDescription(): string;
  setDescription(value: string): void;

  clearColorCodesList(): void;
  getColorCodesList(): Array<ColorCode>;
  setColorCodesList(value: Array<ColorCode>): void;
  addColorCodes(value?: ColorCode, index?: number): ColorCode;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): AddRequest.AsObject;
  static toObject(includeInstance: boolean, msg: AddRequest): AddRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: AddRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): AddRequest;
  static deserializeBinaryFromReader(message: AddRequest, reader: jspb.BinaryReader): AddRequest;
}

export namespace AddRequest {
  export type AsObject = {
    name: string,
    description: string,
    colorCodesList: Array<ColorCode.AsObject>,
  }
}

export class AddResponse extends jspb.Message {
  hasTheme(): boolean;
  clearTheme(): void;
  getTheme(): Theme | undefined;
  setTheme(value?: Theme): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): AddResponse.AsObject;
  static toObject(includeInstance: boolean, msg: AddResponse): AddResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: AddResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): AddResponse;
  static deserializeBinaryFromReader(message: AddResponse, reader: jspb.BinaryReader): AddResponse;
}

export namespace AddResponse {
  export type AsObject = {
    theme?: Theme.AsObject,
  }
}

