import * as $protobuf from "protobufjs";
/** ReplyType enum. */
namespace ReplyType {
  /** Overwrite value */
  let Overwrite: number;

  /** Rename value */
  let Rename: number;
}

/** Represents a TaskSuggestion. */
export class TaskSuggestion implements ITaskSuggestion {
  /**
   * Constructs a new TaskSuggestion.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskSuggestion);

  /** TaskSuggestion suggestions. */
  public suggestions: ReplyType[];

  /** TaskSuggestion itemName. */
  public itemName: string;

  /** TaskSuggestion taskId. */
  public taskId: string;

  /**
   * Creates a new TaskSuggestion instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskSuggestion instance
   */
  public static create(properties?: ITaskSuggestion): TaskSuggestion;

  /**
   * Encodes the specified TaskSuggestion message. Does not implicitly {@link TaskSuggestion.verify|verify} messages.
   * @param message TaskSuggestion message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskSuggestion, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskSuggestion message, length delimited. Does not implicitly {@link TaskSuggestion.verify|verify} messages.
   * @param message TaskSuggestion message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskSuggestion, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskSuggestion message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskSuggestion
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskSuggestion;

  /**
   * Decodes a TaskSuggestion message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskSuggestion
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskSuggestion;

  /**
   * Verifies a TaskSuggestion message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskSuggestion message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskSuggestion
   */
  public static fromObject(object: { [k: string]: any }): TaskSuggestion;

  /**
   * Creates a plain object from a TaskSuggestion message. Also converts values to other types if specified.
   * @param message TaskSuggestion
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: TaskSuggestion, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this TaskSuggestion to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskReplyToOverwriteRequest. */
export class TaskReplyToOverwriteRequest implements ITaskReplyToOverwriteRequest {
  /**
   * Constructs a new TaskReplyToOverwriteRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskReplyToOverwriteRequest);

  /** TaskReplyToOverwriteRequest taskId. */
  public taskId: string;

  /** TaskReplyToOverwriteRequest overwrite. */
  public overwrite: boolean;

  /**
   * Creates a new TaskReplyToOverwriteRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskReplyToOverwriteRequest instance
   */
  public static create(properties?: ITaskReplyToOverwriteRequest): TaskReplyToOverwriteRequest;

  /**
   * Encodes the specified TaskReplyToOverwriteRequest message. Does not implicitly {@link TaskReplyToOverwriteRequest.verify|verify} messages.
   * @param message TaskReplyToOverwriteRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskReplyToOverwriteRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskReplyToOverwriteRequest message, length delimited. Does not implicitly {@link TaskReplyToOverwriteRequest.verify|verify} messages.
   * @param message TaskReplyToOverwriteRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskReplyToOverwriteRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskReplyToOverwriteRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskReplyToOverwriteRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskReplyToOverwriteRequest;

  /**
   * Decodes a TaskReplyToOverwriteRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskReplyToOverwriteRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskReplyToOverwriteRequest;

  /**
   * Verifies a TaskReplyToOverwriteRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskReplyToOverwriteRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskReplyToOverwriteRequest
   */
  public static fromObject(object: { [k: string]: any }): TaskReplyToOverwriteRequest;

  /**
   * Creates a plain object from a TaskReplyToOverwriteRequest message. Also converts values to other types if specified.
   * @param message TaskReplyToOverwriteRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: TaskReplyToOverwriteRequest,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this TaskReplyToOverwriteRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskReplyToOverwriteResponse. */
export class TaskReplyToOverwriteResponse implements ITaskReplyToOverwriteResponse {
  /**
   * Constructs a new TaskReplyToOverwriteResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskReplyToOverwriteResponse);

  /**
   * Creates a new TaskReplyToOverwriteResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskReplyToOverwriteResponse instance
   */
  public static create(properties?: ITaskReplyToOverwriteResponse): TaskReplyToOverwriteResponse;

  /**
   * Encodes the specified TaskReplyToOverwriteResponse message. Does not implicitly {@link TaskReplyToOverwriteResponse.verify|verify} messages.
   * @param message TaskReplyToOverwriteResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskReplyToOverwriteResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskReplyToOverwriteResponse message, length delimited. Does not implicitly {@link TaskReplyToOverwriteResponse.verify|verify} messages.
   * @param message TaskReplyToOverwriteResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskReplyToOverwriteResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskReplyToOverwriteResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskReplyToOverwriteResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskReplyToOverwriteResponse;

  /**
   * Decodes a TaskReplyToOverwriteResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskReplyToOverwriteResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskReplyToOverwriteResponse;

  /**
   * Verifies a TaskReplyToOverwriteResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskReplyToOverwriteResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskReplyToOverwriteResponse
   */
  public static fromObject(object: { [k: string]: any }): TaskReplyToOverwriteResponse;

  /**
   * Creates a plain object from a TaskReplyToOverwriteResponse message. Also converts values to other types if specified.
   * @param message TaskReplyToOverwriteResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: TaskReplyToOverwriteResponse,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this TaskReplyToOverwriteResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskReplyToRenameRequest. */
export class TaskReplyToRenameRequest implements ITaskReplyToRenameRequest {
  /**
   * Constructs a new TaskReplyToRenameRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskReplyToRenameRequest);

  /** TaskReplyToRenameRequest taskId. */
  public taskId: string;

  /** TaskReplyToRenameRequest newName. */
  public newName: string;

  /**
   * Creates a new TaskReplyToRenameRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskReplyToRenameRequest instance
   */
  public static create(properties?: ITaskReplyToRenameRequest): TaskReplyToRenameRequest;

  /**
   * Encodes the specified TaskReplyToRenameRequest message. Does not implicitly {@link TaskReplyToRenameRequest.verify|verify} messages.
   * @param message TaskReplyToRenameRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskReplyToRenameRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskReplyToRenameRequest message, length delimited. Does not implicitly {@link TaskReplyToRenameRequest.verify|verify} messages.
   * @param message TaskReplyToRenameRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskReplyToRenameRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskReplyToRenameRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskReplyToRenameRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskReplyToRenameRequest;

  /**
   * Decodes a TaskReplyToRenameRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskReplyToRenameRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskReplyToRenameRequest;

  /**
   * Verifies a TaskReplyToRenameRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskReplyToRenameRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskReplyToRenameRequest
   */
  public static fromObject(object: { [k: string]: any }): TaskReplyToRenameRequest;

  /**
   * Creates a plain object from a TaskReplyToRenameRequest message. Also converts values to other types if specified.
   * @param message TaskReplyToRenameRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: TaskReplyToRenameRequest,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this TaskReplyToRenameRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskReplyToRenameResponse. */
export class TaskReplyToRenameResponse implements ITaskReplyToRenameResponse {
  /**
   * Constructs a new TaskReplyToRenameResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskReplyToRenameResponse);

  /**
   * Creates a new TaskReplyToRenameResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskReplyToRenameResponse instance
   */
  public static create(properties?: ITaskReplyToRenameResponse): TaskReplyToRenameResponse;

  /**
   * Encodes the specified TaskReplyToRenameResponse message. Does not implicitly {@link TaskReplyToRenameResponse.verify|verify} messages.
   * @param message TaskReplyToRenameResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskReplyToRenameResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskReplyToRenameResponse message, length delimited. Does not implicitly {@link TaskReplyToRenameResponse.verify|verify} messages.
   * @param message TaskReplyToRenameResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskReplyToRenameResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskReplyToRenameResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskReplyToRenameResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskReplyToRenameResponse;

  /**
   * Decodes a TaskReplyToRenameResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskReplyToRenameResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskReplyToRenameResponse;

  /**
   * Verifies a TaskReplyToRenameResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskReplyToRenameResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskReplyToRenameResponse
   */
  public static fromObject(object: { [k: string]: any }): TaskReplyToRenameResponse;

  /**
   * Creates a plain object from a TaskReplyToRenameResponse message. Also converts values to other types if specified.
   * @param message TaskReplyToRenameResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: TaskReplyToRenameResponse,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this TaskReplyToRenameResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskCancelRequest. */
export class TaskCancelRequest implements ITaskCancelRequest {
  /**
   * Constructs a new TaskCancelRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskCancelRequest);

  /** TaskCancelRequest taskId. */
  public taskId: string;

  /**
   * Creates a new TaskCancelRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskCancelRequest instance
   */
  public static create(properties?: ITaskCancelRequest): TaskCancelRequest;

  /**
   * Encodes the specified TaskCancelRequest message. Does not implicitly {@link TaskCancelRequest.verify|verify} messages.
   * @param message TaskCancelRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskCancelRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskCancelRequest message, length delimited. Does not implicitly {@link TaskCancelRequest.verify|verify} messages.
   * @param message TaskCancelRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskCancelRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskCancelRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskCancelRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskCancelRequest;

  /**
   * Decodes a TaskCancelRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskCancelRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskCancelRequest;

  /**
   * Verifies a TaskCancelRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskCancelRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskCancelRequest
   */
  public static fromObject(object: { [k: string]: any }): TaskCancelRequest;

  /**
   * Creates a plain object from a TaskCancelRequest message. Also converts values to other types if specified.
   * @param message TaskCancelRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: TaskCancelRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this TaskCancelRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskCancelResponse. */
export class TaskCancelResponse implements ITaskCancelResponse {
  /**
   * Constructs a new TaskCancelResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskCancelResponse);

  /**
   * Creates a new TaskCancelResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskCancelResponse instance
   */
  public static create(properties?: ITaskCancelResponse): TaskCancelResponse;

  /**
   * Encodes the specified TaskCancelResponse message. Does not implicitly {@link TaskCancelResponse.verify|verify} messages.
   * @param message TaskCancelResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskCancelResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskCancelResponse message, length delimited. Does not implicitly {@link TaskCancelResponse.verify|verify} messages.
   * @param message TaskCancelResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskCancelResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskCancelResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskCancelResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskCancelResponse;

  /**
   * Decodes a TaskCancelResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskCancelResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskCancelResponse;

  /**
   * Verifies a TaskCancelResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskCancelResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskCancelResponse
   */
  public static fromObject(object: { [k: string]: any }): TaskCancelResponse;

  /**
   * Creates a plain object from a TaskCancelResponse message. Also converts values to other types if specified.
   * @param message TaskCancelResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: TaskCancelResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this TaskCancelResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}
