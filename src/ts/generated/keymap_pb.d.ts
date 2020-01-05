import * as $protobuf from "protobufjs";
/** Properties of a Keymap. */
export interface IKeymap {
  /** Keymap bindings */
  bindings?: IBinding[] | null;
}

/** Represents a Keymap. */
export class Keymap implements IKeymap {
  /**
   * Constructs a new Keymap.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymap);

  /** Keymap bindings. */
  public bindings: IBinding[];

  /**
   * Creates a new Keymap instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Keymap instance
   */
  public static create(properties?: IKeymap): Keymap;

  /**
   * Encodes the specified Keymap message. Does not implicitly {@link Keymap.verify|verify} messages.
   * @param message Keymap message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymap, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Keymap message, length delimited. Does not implicitly {@link Keymap.verify|verify} messages.
   * @param message Keymap message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymap, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Keymap message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Keymap
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Keymap;

  /**
   * Decodes a Keymap message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Keymap
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Keymap;

  /**
   * Verifies a Keymap message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Keymap message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Keymap
   */
  public static fromObject(object: { [k: string]: any }): Keymap;

  /**
   * Creates a plain object from a Keymap message. Also converts values to other types if specified.
   * @param message Keymap
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Keymap, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Keymap to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a Binding. */
export interface IBinding {
  /** Binding key */
  key?: string | null;

  /** Binding action */
  action?: string | null;

  /** Binding contexts */
  contexts?: string[] | null;
}

/** Represents a Binding. */
export class Binding implements IBinding {
  /**
   * Constructs a new Binding.
   * @param [properties] Properties to set
   */
  constructor(properties?: IBinding);

  /** Binding key. */
  public key: string;

  /** Binding action. */
  public action: string;

  /** Binding contexts. */
  public contexts: string[];

  /**
   * Creates a new Binding instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Binding instance
   */
  public static create(properties?: IBinding): Binding;

  /**
   * Encodes the specified Binding message. Does not implicitly {@link Binding.verify|verify} messages.
   * @param message Binding message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IBinding, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Binding message, length delimited. Does not implicitly {@link Binding.verify|verify} messages.
   * @param message Binding message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IBinding, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Binding message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Binding
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Binding;

  /**
   * Decodes a Binding message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Binding
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Binding;

  /**
   * Verifies a Binding message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Binding message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Binding
   */
  public static fromObject(object: { [k: string]: any }): Binding;

  /**
   * Creates a plain object from a Binding message. Also converts values to other types if specified.
   * @param message Binding
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Binding, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Binding to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a KeymapGetRequest. */
export interface IKeymapGetRequest {}

/** Represents a KeymapGetRequest. */
export class KeymapGetRequest implements IKeymapGetRequest {
  /**
   * Constructs a new KeymapGetRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymapGetRequest);

  /**
   * Creates a new KeymapGetRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns KeymapGetRequest instance
   */
  public static create(properties?: IKeymapGetRequest): KeymapGetRequest;

  /**
   * Encodes the specified KeymapGetRequest message. Does not implicitly {@link KeymapGetRequest.verify|verify} messages.
   * @param message KeymapGetRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymapGetRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified KeymapGetRequest message, length delimited. Does not implicitly {@link KeymapGetRequest.verify|verify} messages.
   * @param message KeymapGetRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymapGetRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a KeymapGetRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns KeymapGetRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): KeymapGetRequest;

  /**
   * Decodes a KeymapGetRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns KeymapGetRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): KeymapGetRequest;

  /**
   * Verifies a KeymapGetRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a KeymapGetRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns KeymapGetRequest
   */
  public static fromObject(object: { [k: string]: any }): KeymapGetRequest;

  /**
   * Creates a plain object from a KeymapGetRequest message. Also converts values to other types if specified.
   * @param message KeymapGetRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: KeymapGetRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this KeymapGetRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a KeymapGetResponse. */
export interface IKeymapGetResponse {
  /** KeymapGetResponse keymap */
  keymap?: IKeymap | null;
}

/** Represents a KeymapGetResponse. */
export class KeymapGetResponse implements IKeymapGetResponse {
  /**
   * Constructs a new KeymapGetResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymapGetResponse);

  /** KeymapGetResponse keymap. */
  public keymap?: IKeymap | null;

  /**
   * Creates a new KeymapGetResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns KeymapGetResponse instance
   */
  public static create(properties?: IKeymapGetResponse): KeymapGetResponse;

  /**
   * Encodes the specified KeymapGetResponse message. Does not implicitly {@link KeymapGetResponse.verify|verify} messages.
   * @param message KeymapGetResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymapGetResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified KeymapGetResponse message, length delimited. Does not implicitly {@link KeymapGetResponse.verify|verify} messages.
   * @param message KeymapGetResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymapGetResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a KeymapGetResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns KeymapGetResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): KeymapGetResponse;

  /**
   * Decodes a KeymapGetResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns KeymapGetResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): KeymapGetResponse;

  /**
   * Verifies a KeymapGetResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a KeymapGetResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns KeymapGetResponse
   */
  public static fromObject(object: { [k: string]: any }): KeymapGetResponse;

  /**
   * Creates a plain object from a KeymapGetResponse message. Also converts values to other types if specified.
   * @param message KeymapGetResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: KeymapGetResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this KeymapGetResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a KeymapReloadRequest. */
export interface IKeymapReloadRequest {}

/** Represents a KeymapReloadRequest. */
export class KeymapReloadRequest implements IKeymapReloadRequest {
  /**
   * Constructs a new KeymapReloadRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymapReloadRequest);

  /**
   * Creates a new KeymapReloadRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns KeymapReloadRequest instance
   */
  public static create(properties?: IKeymapReloadRequest): KeymapReloadRequest;

  /**
   * Encodes the specified KeymapReloadRequest message. Does not implicitly {@link KeymapReloadRequest.verify|verify} messages.
   * @param message KeymapReloadRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymapReloadRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified KeymapReloadRequest message, length delimited. Does not implicitly {@link KeymapReloadRequest.verify|verify} messages.
   * @param message KeymapReloadRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymapReloadRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a KeymapReloadRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns KeymapReloadRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): KeymapReloadRequest;

  /**
   * Decodes a KeymapReloadRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns KeymapReloadRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): KeymapReloadRequest;

  /**
   * Verifies a KeymapReloadRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a KeymapReloadRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns KeymapReloadRequest
   */
  public static fromObject(object: { [k: string]: any }): KeymapReloadRequest;

  /**
   * Creates a plain object from a KeymapReloadRequest message. Also converts values to other types if specified.
   * @param message KeymapReloadRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: KeymapReloadRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this KeymapReloadRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a KeymapReloadResponse. */
export interface IKeymapReloadResponse {
  /** KeymapReloadResponse keymap */
  keymap?: IKeymap | null;
}

/** Represents a KeymapReloadResponse. */
export class KeymapReloadResponse implements IKeymapReloadResponse {
  /**
   * Constructs a new KeymapReloadResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymapReloadResponse);

  /** KeymapReloadResponse keymap. */
  public keymap?: IKeymap | null;

  /**
   * Creates a new KeymapReloadResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns KeymapReloadResponse instance
   */
  public static create(properties?: IKeymapReloadResponse): KeymapReloadResponse;

  /**
   * Encodes the specified KeymapReloadResponse message. Does not implicitly {@link KeymapReloadResponse.verify|verify} messages.
   * @param message KeymapReloadResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymapReloadResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified KeymapReloadResponse message, length delimited. Does not implicitly {@link KeymapReloadResponse.verify|verify} messages.
   * @param message KeymapReloadResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymapReloadResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a KeymapReloadResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns KeymapReloadResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): KeymapReloadResponse;

  /**
   * Decodes a KeymapReloadResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns KeymapReloadResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): KeymapReloadResponse;

  /**
   * Verifies a KeymapReloadResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a KeymapReloadResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns KeymapReloadResponse
   */
  public static fromObject(object: { [k: string]: any }): KeymapReloadResponse;

  /**
   * Creates a plain object from a KeymapReloadResponse message. Also converts values to other types if specified.
   * @param message KeymapReloadResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: KeymapReloadResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this KeymapReloadResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a KeymapStoreRequest. */
export interface IKeymapStoreRequest {
  /** KeymapStoreRequest keymap */
  keymap?: IKeymap | null;
}

/** Represents a KeymapStoreRequest. */
export class KeymapStoreRequest implements IKeymapStoreRequest {
  /**
   * Constructs a new KeymapStoreRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymapStoreRequest);

  /** KeymapStoreRequest keymap. */
  public keymap?: IKeymap | null;

  /**
   * Creates a new KeymapStoreRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns KeymapStoreRequest instance
   */
  public static create(properties?: IKeymapStoreRequest): KeymapStoreRequest;

  /**
   * Encodes the specified KeymapStoreRequest message. Does not implicitly {@link KeymapStoreRequest.verify|verify} messages.
   * @param message KeymapStoreRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymapStoreRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified KeymapStoreRequest message, length delimited. Does not implicitly {@link KeymapStoreRequest.verify|verify} messages.
   * @param message KeymapStoreRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymapStoreRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a KeymapStoreRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns KeymapStoreRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): KeymapStoreRequest;

  /**
   * Decodes a KeymapStoreRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns KeymapStoreRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): KeymapStoreRequest;

  /**
   * Verifies a KeymapStoreRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a KeymapStoreRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns KeymapStoreRequest
   */
  public static fromObject(object: { [k: string]: any }): KeymapStoreRequest;

  /**
   * Creates a plain object from a KeymapStoreRequest message. Also converts values to other types if specified.
   * @param message KeymapStoreRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: KeymapStoreRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this KeymapStoreRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a KeymapStoreResponse. */
export interface IKeymapStoreResponse {}

/** Represents a KeymapStoreResponse. */
export class KeymapStoreResponse implements IKeymapStoreResponse {
  /**
   * Constructs a new KeymapStoreResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IKeymapStoreResponse);

  /**
   * Creates a new KeymapStoreResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns KeymapStoreResponse instance
   */
  public static create(properties?: IKeymapStoreResponse): KeymapStoreResponse;

  /**
   * Encodes the specified KeymapStoreResponse message. Does not implicitly {@link KeymapStoreResponse.verify|verify} messages.
   * @param message KeymapStoreResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IKeymapStoreResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified KeymapStoreResponse message, length delimited. Does not implicitly {@link KeymapStoreResponse.verify|verify} messages.
   * @param message KeymapStoreResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IKeymapStoreResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a KeymapStoreResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns KeymapStoreResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): KeymapStoreResponse;

  /**
   * Decodes a KeymapStoreResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns KeymapStoreResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): KeymapStoreResponse;

  /**
   * Verifies a KeymapStoreResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a KeymapStoreResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns KeymapStoreResponse
   */
  public static fromObject(object: { [k: string]: any }): KeymapStoreResponse;

  /**
   * Creates a plain object from a KeymapStoreResponse message. Also converts values to other types if specified.
   * @param message KeymapStoreResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: KeymapStoreResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this KeymapStoreResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}
