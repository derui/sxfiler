import * as $protobuf from "protobufjs";
/** Properties of a Configuration. */
export interface IConfiguration {
  /** Configuration defaultSortOrder */
  defaultSortOrder?: SortType | null;
}

/** Represents a Configuration. */
export class Configuration implements IConfiguration {
  /**
   * Constructs a new Configuration.
   * @param [properties] Properties to set
   */
  constructor(properties?: IConfiguration);

  /** Configuration defaultSortOrder. */
  public defaultSortOrder: SortType;

  /**
   * Creates a new Configuration instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Configuration instance
   */
  public static create(properties?: IConfiguration): Configuration;

  /**
   * Encodes the specified Configuration message. Does not implicitly {@link Configuration.verify|verify} messages.
   * @param message Configuration message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IConfiguration, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Configuration message, length delimited. Does not implicitly {@link Configuration.verify|verify} messages.
   * @param message Configuration message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IConfiguration, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Configuration message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Configuration
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Configuration;

  /**
   * Decodes a Configuration message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Configuration
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Configuration;

  /**
   * Verifies a Configuration message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Configuration message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Configuration
   */
  public static fromObject(object: { [k: string]: any }): Configuration;

  /**
   * Creates a plain object from a Configuration message. Also converts values to other types if specified.
   * @param message Configuration
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Configuration, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Configuration to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a GetRequest. */
export interface IGetRequest {}

/** Represents a GetRequest. */
export class GetRequest implements IGetRequest {
  /**
   * Constructs a new GetRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IGetRequest);

  /**
   * Creates a new GetRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns GetRequest instance
   */
  public static create(properties?: IGetRequest): GetRequest;

  /**
   * Encodes the specified GetRequest message. Does not implicitly {@link GetRequest.verify|verify} messages.
   * @param message GetRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IGetRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified GetRequest message, length delimited. Does not implicitly {@link GetRequest.verify|verify} messages.
   * @param message GetRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IGetRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a GetRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns GetRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): GetRequest;

  /**
   * Decodes a GetRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns GetRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): GetRequest;

  /**
   * Verifies a GetRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a GetRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns GetRequest
   */
  public static fromObject(object: { [k: string]: any }): GetRequest;

  /**
   * Creates a plain object from a GetRequest message. Also converts values to other types if specified.
   * @param message GetRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: GetRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this GetRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a GetResponse. */
export interface IGetResponse {
  /** GetResponse configuration */
  configuration?: IConfiguration | null;
}

/** Represents a GetResponse. */
export class GetResponse implements IGetResponse {
  /**
   * Constructs a new GetResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IGetResponse);

  /** GetResponse configuration. */
  public configuration?: IConfiguration | null;

  /**
   * Creates a new GetResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns GetResponse instance
   */
  public static create(properties?: IGetResponse): GetResponse;

  /**
   * Encodes the specified GetResponse message. Does not implicitly {@link GetResponse.verify|verify} messages.
   * @param message GetResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IGetResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified GetResponse message, length delimited. Does not implicitly {@link GetResponse.verify|verify} messages.
   * @param message GetResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IGetResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a GetResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns GetResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): GetResponse;

  /**
   * Decodes a GetResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns GetResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): GetResponse;

  /**
   * Verifies a GetResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a GetResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns GetResponse
   */
  public static fromObject(object: { [k: string]: any }): GetResponse;

  /**
   * Creates a plain object from a GetResponse message. Also converts values to other types if specified.
   * @param message GetResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: GetResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this GetResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a StoreRequest. */
export interface IStoreRequest {
  /** StoreRequest configuration */
  configuration?: IConfiguration | null;
}

/** Represents a StoreRequest. */
export class StoreRequest implements IStoreRequest {
  /**
   * Constructs a new StoreRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IStoreRequest);

  /** StoreRequest configuration. */
  public configuration?: IConfiguration | null;

  /**
   * Creates a new StoreRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns StoreRequest instance
   */
  public static create(properties?: IStoreRequest): StoreRequest;

  /**
   * Encodes the specified StoreRequest message. Does not implicitly {@link StoreRequest.verify|verify} messages.
   * @param message StoreRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IStoreRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified StoreRequest message, length delimited. Does not implicitly {@link StoreRequest.verify|verify} messages.
   * @param message StoreRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IStoreRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a StoreRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns StoreRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): StoreRequest;

  /**
   * Decodes a StoreRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns StoreRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): StoreRequest;

  /**
   * Verifies a StoreRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a StoreRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns StoreRequest
   */
  public static fromObject(object: { [k: string]: any }): StoreRequest;

  /**
   * Creates a plain object from a StoreRequest message. Also converts values to other types if specified.
   * @param message StoreRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: StoreRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this StoreRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a StoreResponse. */
export interface IStoreResponse {}

/** Represents a StoreResponse. */
export class StoreResponse implements IStoreResponse {
  /**
   * Constructs a new StoreResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IStoreResponse);

  /**
   * Creates a new StoreResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns StoreResponse instance
   */
  public static create(properties?: IStoreResponse): StoreResponse;

  /**
   * Encodes the specified StoreResponse message. Does not implicitly {@link StoreResponse.verify|verify} messages.
   * @param message StoreResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IStoreResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified StoreResponse message, length delimited. Does not implicitly {@link StoreResponse.verify|verify} messages.
   * @param message StoreResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IStoreResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a StoreResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns StoreResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): StoreResponse;

  /**
   * Decodes a StoreResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns StoreResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): StoreResponse;

  /**
   * Verifies a StoreResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a StoreResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns StoreResponse
   */
  public static fromObject(object: { [k: string]: any }): StoreResponse;

  /**
   * Creates a plain object from a StoreResponse message. Also converts values to other types if specified.
   * @param message StoreResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: StoreResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this StoreResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}
