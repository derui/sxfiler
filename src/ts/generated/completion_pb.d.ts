import * as $protobuf from "protobufjs";
/** Properties of an Item. */
export interface IItem {
  /** Item id */
  id?: string | null;

  /** Item value */
  value?: string | null;
}

/** Represents an Item. */
export class Item implements IItem {
  /**
   * Constructs a new Item.
   * @param [properties] Properties to set
   */
  constructor(properties?: IItem);

  /** Item id. */
  public id: string;

  /** Item value. */
  public value: string;

  /**
   * Creates a new Item instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Item instance
   */
  public static create(properties?: IItem): Item;

  /**
   * Encodes the specified Item message. Does not implicitly {@link Item.verify|verify} messages.
   * @param message Item message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IItem, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Item message, length delimited. Does not implicitly {@link Item.verify|verify} messages.
   * @param message Item message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IItem, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes an Item message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Item
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Item;

  /**
   * Decodes an Item message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Item
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Item;

  /**
   * Verifies an Item message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates an Item message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Item
   */
  public static fromObject(object: { [k: string]: any }): Item;

  /**
   * Creates a plain object from an Item message. Also converts values to other types if specified.
   * @param message Item
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Item, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Item to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a Candidate. */
export interface ICandidate {
  /** Candidate start */
  start?: number | null;

  /** Candidate length */
  length?: number | null;

  /** Candidate value */
  value?: IItem | null;
}

/** Represents a Candidate. */
export class Candidate implements ICandidate {
  /**
   * Constructs a new Candidate.
   * @param [properties] Properties to set
   */
  constructor(properties?: ICandidate);

  /** Candidate start. */
  public start: number;

  /** Candidate length. */
  public length: number;

  /** Candidate value. */
  public value?: IItem | null;

  /**
   * Creates a new Candidate instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Candidate instance
   */
  public static create(properties?: ICandidate): Candidate;

  /**
   * Encodes the specified Candidate message. Does not implicitly {@link Candidate.verify|verify} messages.
   * @param message Candidate message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ICandidate, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Candidate message, length delimited. Does not implicitly {@link Candidate.verify|verify} messages.
   * @param message Candidate message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ICandidate, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Candidate message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Candidate
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Candidate;

  /**
   * Decodes a Candidate message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Candidate
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Candidate;

  /**
   * Verifies a Candidate message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Candidate message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Candidate
   */
  public static fromObject(object: { [k: string]: any }): Candidate;

  /**
   * Creates a plain object from a Candidate message. Also converts values to other types if specified.
   * @param message Candidate
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Candidate, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Candidate to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a SetupRequest. */
export interface ISetupRequest {
  /** SetupRequest source */
  source?: IItem[] | null;
}

/** Represents a SetupRequest. */
export class SetupRequest implements ISetupRequest {
  /**
   * Constructs a new SetupRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: ISetupRequest);

  /** SetupRequest source. */
  public source: IItem[];

  /**
   * Creates a new SetupRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns SetupRequest instance
   */
  public static create(properties?: ISetupRequest): SetupRequest;

  /**
   * Encodes the specified SetupRequest message. Does not implicitly {@link SetupRequest.verify|verify} messages.
   * @param message SetupRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ISetupRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified SetupRequest message, length delimited. Does not implicitly {@link SetupRequest.verify|verify} messages.
   * @param message SetupRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ISetupRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a SetupRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns SetupRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): SetupRequest;

  /**
   * Decodes a SetupRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns SetupRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): SetupRequest;

  /**
   * Verifies a SetupRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a SetupRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns SetupRequest
   */
  public static fromObject(object: { [k: string]: any }): SetupRequest;

  /**
   * Creates a plain object from a SetupRequest message. Also converts values to other types if specified.
   * @param message SetupRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: SetupRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this SetupRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a SetupResponse. */
export interface ISetupResponse {}

/** Represents a SetupResponse. */
export class SetupResponse implements ISetupResponse {
  /**
   * Constructs a new SetupResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: ISetupResponse);

  /**
   * Creates a new SetupResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns SetupResponse instance
   */
  public static create(properties?: ISetupResponse): SetupResponse;

  /**
   * Encodes the specified SetupResponse message. Does not implicitly {@link SetupResponse.verify|verify} messages.
   * @param message SetupResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ISetupResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified SetupResponse message, length delimited. Does not implicitly {@link SetupResponse.verify|verify} messages.
   * @param message SetupResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ISetupResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a SetupResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns SetupResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): SetupResponse;

  /**
   * Decodes a SetupResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns SetupResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): SetupResponse;

  /**
   * Verifies a SetupResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a SetupResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns SetupResponse
   */
  public static fromObject(object: { [k: string]: any }): SetupResponse;

  /**
   * Creates a plain object from a SetupResponse message. Also converts values to other types if specified.
   * @param message SetupResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: SetupResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this SetupResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a ReadRequest. */
export interface IReadRequest {
  /** ReadRequest input */
  input?: string | null;
}

/** Represents a ReadRequest. */
export class ReadRequest implements IReadRequest {
  /**
   * Constructs a new ReadRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IReadRequest);

  /** ReadRequest input. */
  public input: string;

  /**
   * Creates a new ReadRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns ReadRequest instance
   */
  public static create(properties?: IReadRequest): ReadRequest;

  /**
   * Encodes the specified ReadRequest message. Does not implicitly {@link ReadRequest.verify|verify} messages.
   * @param message ReadRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IReadRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified ReadRequest message, length delimited. Does not implicitly {@link ReadRequest.verify|verify} messages.
   * @param message ReadRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IReadRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a ReadRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns ReadRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): ReadRequest;

  /**
   * Decodes a ReadRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns ReadRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): ReadRequest;

  /**
   * Verifies a ReadRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a ReadRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns ReadRequest
   */
  public static fromObject(object: { [k: string]: any }): ReadRequest;

  /**
   * Creates a plain object from a ReadRequest message. Also converts values to other types if specified.
   * @param message ReadRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: ReadRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this ReadRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a ReadResponse. */
export interface IReadResponse {
  /** ReadResponse candidates */
  candidates?: ICandidate[] | null;
}

/** Represents a ReadResponse. */
export class ReadResponse implements IReadResponse {
  /**
   * Constructs a new ReadResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IReadResponse);

  /** ReadResponse candidates. */
  public candidates: ICandidate[];

  /**
   * Creates a new ReadResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns ReadResponse instance
   */
  public static create(properties?: IReadResponse): ReadResponse;

  /**
   * Encodes the specified ReadResponse message. Does not implicitly {@link ReadResponse.verify|verify} messages.
   * @param message ReadResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IReadResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified ReadResponse message, length delimited. Does not implicitly {@link ReadResponse.verify|verify} messages.
   * @param message ReadResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IReadResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a ReadResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns ReadResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): ReadResponse;

  /**
   * Decodes a ReadResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns ReadResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): ReadResponse;

  /**
   * Verifies a ReadResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a ReadResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns ReadResponse
   */
  public static fromObject(object: { [k: string]: any }): ReadResponse;

  /**
   * Creates a plain object from a ReadResponse message. Also converts values to other types if specified.
   * @param message ReadResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: ReadResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this ReadResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}
