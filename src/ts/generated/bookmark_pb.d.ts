import * as $protobuf from "protobufjs";
/** Properties of a Bookmark. */
export interface IBookmark {
  /** Bookmark id */
  id?: string | null;

  /** Bookmark path */
  path?: string | null;

  /** Bookmark order */
  order?: number | null;
}

/** Represents a Bookmark. */
export class Bookmark implements IBookmark {
  /**
   * Constructs a new Bookmark.
   * @param [properties] Properties to set
   */
  constructor(properties?: IBookmark);

  /** Bookmark id. */
  public id: string;

  /** Bookmark path. */
  public path: string;

  /** Bookmark order. */
  public order: number;

  /**
   * Creates a new Bookmark instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Bookmark instance
   */
  public static create(properties?: IBookmark): Bookmark;

  /**
   * Encodes the specified Bookmark message. Does not implicitly {@link Bookmark.verify|verify} messages.
   * @param message Bookmark message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IBookmark, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Bookmark message, length delimited. Does not implicitly {@link Bookmark.verify|verify} messages.
   * @param message Bookmark message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IBookmark, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Bookmark message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Bookmark
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Bookmark;

  /**
   * Decodes a Bookmark message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Bookmark
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Bookmark;

  /**
   * Verifies a Bookmark message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Bookmark message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Bookmark
   */
  public static fromObject(object: { [k: string]: any }): Bookmark;

  /**
   * Creates a plain object from a Bookmark message. Also converts values to other types if specified.
   * @param message Bookmark
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Bookmark, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Bookmark to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a ListAllRequest. */
export interface IListAllRequest {}

/** Represents a ListAllRequest. */
export class ListAllRequest implements IListAllRequest {
  /**
   * Constructs a new ListAllRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IListAllRequest);

  /**
   * Creates a new ListAllRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns ListAllRequest instance
   */
  public static create(properties?: IListAllRequest): ListAllRequest;

  /**
   * Encodes the specified ListAllRequest message. Does not implicitly {@link ListAllRequest.verify|verify} messages.
   * @param message ListAllRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IListAllRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified ListAllRequest message, length delimited. Does not implicitly {@link ListAllRequest.verify|verify} messages.
   * @param message ListAllRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IListAllRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a ListAllRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns ListAllRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): ListAllRequest;

  /**
   * Decodes a ListAllRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns ListAllRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): ListAllRequest;

  /**
   * Verifies a ListAllRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a ListAllRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns ListAllRequest
   */
  public static fromObject(object: { [k: string]: any }): ListAllRequest;

  /**
   * Creates a plain object from a ListAllRequest message. Also converts values to other types if specified.
   * @param message ListAllRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: ListAllRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this ListAllRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a ListAllResponse. */
export interface IListAllResponse {
  /** ListAllResponse bookmarks */
  bookmarks?: IBookmark[] | null;
}

/** Represents a ListAllResponse. */
export class ListAllResponse implements IListAllResponse {
  /**
   * Constructs a new ListAllResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IListAllResponse);

  /** ListAllResponse bookmarks. */
  public bookmarks: IBookmark[];

  /**
   * Creates a new ListAllResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns ListAllResponse instance
   */
  public static create(properties?: IListAllResponse): ListAllResponse;

  /**
   * Encodes the specified ListAllResponse message. Does not implicitly {@link ListAllResponse.verify|verify} messages.
   * @param message ListAllResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IListAllResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified ListAllResponse message, length delimited. Does not implicitly {@link ListAllResponse.verify|verify} messages.
   * @param message ListAllResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IListAllResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a ListAllResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns ListAllResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): ListAllResponse;

  /**
   * Decodes a ListAllResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns ListAllResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): ListAllResponse;

  /**
   * Verifies a ListAllResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a ListAllResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns ListAllResponse
   */
  public static fromObject(object: { [k: string]: any }): ListAllResponse;

  /**
   * Creates a plain object from a ListAllResponse message. Also converts values to other types if specified.
   * @param message ListAllResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: ListAllResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this ListAllResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a RegisterRequest. */
export interface IRegisterRequest {
  /** RegisterRequest path */
  path?: string | null;
}

/** Represents a RegisterRequest. */
export class RegisterRequest implements IRegisterRequest {
  /**
   * Constructs a new RegisterRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IRegisterRequest);

  /** RegisterRequest path. */
  public path: string;

  /**
   * Creates a new RegisterRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns RegisterRequest instance
   */
  public static create(properties?: IRegisterRequest): RegisterRequest;

  /**
   * Encodes the specified RegisterRequest message. Does not implicitly {@link RegisterRequest.verify|verify} messages.
   * @param message RegisterRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IRegisterRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified RegisterRequest message, length delimited. Does not implicitly {@link RegisterRequest.verify|verify} messages.
   * @param message RegisterRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IRegisterRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a RegisterRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns RegisterRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): RegisterRequest;

  /**
   * Decodes a RegisterRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns RegisterRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): RegisterRequest;

  /**
   * Verifies a RegisterRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a RegisterRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns RegisterRequest
   */
  public static fromObject(object: { [k: string]: any }): RegisterRequest;

  /**
   * Creates a plain object from a RegisterRequest message. Also converts values to other types if specified.
   * @param message RegisterRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: RegisterRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this RegisterRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a RegisterResponse. */
export interface IRegisterResponse {
  /** RegisterResponse bookmark */
  bookmark?: IBookmark | null;
}

/** Represents a RegisterResponse. */
export class RegisterResponse implements IRegisterResponse {
  /**
   * Constructs a new RegisterResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IRegisterResponse);

  /** RegisterResponse bookmark. */
  public bookmark?: IBookmark | null;

  /**
   * Creates a new RegisterResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns RegisterResponse instance
   */
  public static create(properties?: IRegisterResponse): RegisterResponse;

  /**
   * Encodes the specified RegisterResponse message. Does not implicitly {@link RegisterResponse.verify|verify} messages.
   * @param message RegisterResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IRegisterResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified RegisterResponse message, length delimited. Does not implicitly {@link RegisterResponse.verify|verify} messages.
   * @param message RegisterResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IRegisterResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a RegisterResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns RegisterResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): RegisterResponse;

  /**
   * Decodes a RegisterResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns RegisterResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): RegisterResponse;

  /**
   * Verifies a RegisterResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a RegisterResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns RegisterResponse
   */
  public static fromObject(object: { [k: string]: any }): RegisterResponse;

  /**
   * Creates a plain object from a RegisterResponse message. Also converts values to other types if specified.
   * @param message RegisterResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: RegisterResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this RegisterResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a DeleteRequest. */
export interface IDeleteRequest {
  /** DeleteRequest id */
  id?: string | null;
}

/** Represents a DeleteRequest. */
export class DeleteRequest implements IDeleteRequest {
  /**
   * Constructs a new DeleteRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IDeleteRequest);

  /** DeleteRequest id. */
  public id: string;

  /**
   * Creates a new DeleteRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns DeleteRequest instance
   */
  public static create(properties?: IDeleteRequest): DeleteRequest;

  /**
   * Encodes the specified DeleteRequest message. Does not implicitly {@link DeleteRequest.verify|verify} messages.
   * @param message DeleteRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IDeleteRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified DeleteRequest message, length delimited. Does not implicitly {@link DeleteRequest.verify|verify} messages.
   * @param message DeleteRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IDeleteRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a DeleteRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns DeleteRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): DeleteRequest;

  /**
   * Decodes a DeleteRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns DeleteRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): DeleteRequest;

  /**
   * Verifies a DeleteRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a DeleteRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns DeleteRequest
   */
  public static fromObject(object: { [k: string]: any }): DeleteRequest;

  /**
   * Creates a plain object from a DeleteRequest message. Also converts values to other types if specified.
   * @param message DeleteRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: DeleteRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this DeleteRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a DeleteResponse. */
export interface IDeleteResponse {
  /** DeleteResponse deletedBookmark */
  deletedBookmark?: IBookmark | null;
}

/** Represents a DeleteResponse. */
export class DeleteResponse implements IDeleteResponse {
  /**
   * Constructs a new DeleteResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IDeleteResponse);

  /** DeleteResponse deletedBookmark. */
  public deletedBookmark?: IBookmark | null;

  /**
   * Creates a new DeleteResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns DeleteResponse instance
   */
  public static create(properties?: IDeleteResponse): DeleteResponse;

  /**
   * Encodes the specified DeleteResponse message. Does not implicitly {@link DeleteResponse.verify|verify} messages.
   * @param message DeleteResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IDeleteResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified DeleteResponse message, length delimited. Does not implicitly {@link DeleteResponse.verify|verify} messages.
   * @param message DeleteResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IDeleteResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a DeleteResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns DeleteResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): DeleteResponse;

  /**
   * Decodes a DeleteResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns DeleteResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): DeleteResponse;

  /**
   * Verifies a DeleteResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a DeleteResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns DeleteResponse
   */
  public static fromObject(object: { [k: string]: any }): DeleteResponse;

  /**
   * Creates a plain object from a DeleteResponse message. Also converts values to other types if specified.
   * @param message DeleteResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: DeleteResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this DeleteResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}
