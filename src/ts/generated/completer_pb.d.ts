// package: completer
// file: completer.proto

import * as jspb from "google-protobuf";

export class Item extends jspb.Message {
  getId(): string;
  setId(value: string): void;

  getValue(): string;
  setValue(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Item.AsObject;
  static toObject(includeInstance: boolean, msg: Item): Item.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Item, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Item;
  static deserializeBinaryFromReader(message: Item, reader: jspb.BinaryReader): Item;
}

export namespace Item {
  export type AsObject = {
    id: string,
    value: string,
  }
}

export class Candidate extends jspb.Message {
  getStart(): number;
  setStart(value: number): void;

  getLength(): number;
  setLength(value: number): void;

  hasValue(): boolean;
  clearValue(): void;
  getValue(): Item | undefined;
  setValue(value?: Item): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): Candidate.AsObject;
  static toObject(includeInstance: boolean, msg: Candidate): Candidate.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: Candidate, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): Candidate;
  static deserializeBinaryFromReader(message: Candidate, reader: jspb.BinaryReader): Candidate;
}

export namespace Candidate {
  export type AsObject = {
    start: number,
    length: number,
    value?: Item.AsObject,
  }
}

export class InitializeRequest extends jspb.Message {
  clearSourceList(): void;
  getSourceList(): Array<Item>;
  setSourceList(value: Array<Item>): void;
  addSource(value?: Item, index?: number): Item;

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
    sourceList: Array<Item.AsObject>,
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

export class CompleteRequest extends jspb.Message {
  getInput(): string;
  setInput(value: string): void;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CompleteRequest.AsObject;
  static toObject(includeInstance: boolean, msg: CompleteRequest): CompleteRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: CompleteRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CompleteRequest;
  static deserializeBinaryFromReader(message: CompleteRequest, reader: jspb.BinaryReader): CompleteRequest;
}

export namespace CompleteRequest {
  export type AsObject = {
    input: string,
  }
}

export class CompleteResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CompleteResponse.AsObject;
  static toObject(includeInstance: boolean, msg: CompleteResponse): CompleteResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: CompleteResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CompleteResponse;
  static deserializeBinaryFromReader(message: CompleteResponse, reader: jspb.BinaryReader): CompleteResponse;
}

export namespace CompleteResponse {
  export type AsObject = {
  }
}

export class CompletionResultNotificationRequest extends jspb.Message {
  clearCandidatesList(): void;
  getCandidatesList(): Array<Candidate>;
  setCandidatesList(value: Array<Candidate>): void;
  addCandidates(value?: Candidate, index?: number): Candidate;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CompletionResultNotificationRequest.AsObject;
  static toObject(includeInstance: boolean, msg: CompletionResultNotificationRequest): CompletionResultNotificationRequest.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: CompletionResultNotificationRequest, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CompletionResultNotificationRequest;
  static deserializeBinaryFromReader(message: CompletionResultNotificationRequest, reader: jspb.BinaryReader): CompletionResultNotificationRequest;
}

export namespace CompletionResultNotificationRequest {
  export type AsObject = {
    candidatesList: Array<Candidate.AsObject>,
  }
}

export class CompletionResultNotificationResponse extends jspb.Message {
  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CompletionResultNotificationResponse.AsObject;
  static toObject(includeInstance: boolean, msg: CompletionResultNotificationResponse): CompletionResultNotificationResponse.AsObject;
  static extensions: {[key: number]: jspb.ExtensionFieldInfo<jspb.Message>};
  static extensionsBinary: {[key: number]: jspb.ExtensionFieldBinaryInfo<jspb.Message>};
  static serializeBinaryToWriter(message: CompletionResultNotificationResponse, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CompletionResultNotificationResponse;
  static deserializeBinaryFromReader(message: CompletionResultNotificationResponse, reader: jspb.BinaryReader): CompletionResultNotificationResponse;
}

export namespace CompletionResultNotificationResponse {
  export type AsObject = {
  }
}

