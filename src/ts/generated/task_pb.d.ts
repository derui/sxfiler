import * as $protobuf from "protobufjs";
/** ReplyType enum. */
namespace ReplyType {
  /** Overwrite value */
  let Overwrite: number;

  /** Rename value */
  let Rename: number;
}

/** Represents a TaskReply. */
export class TaskReply implements ITaskReply {
  /**
   * Constructs a new TaskReply.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskReply);

  /** TaskReply type. */
  public type: ReplyType;

  /** TaskReply overwrite. */
  public overwrite: boolean;

  /** TaskReply rename. */
  public rename?: TaskReply.IRename | null;

  /** TaskReply taskId. */
  public taskId: string;

  /** TaskReply reply. */
  public reply?: "overwrite" | "rename";

  /**
   * Creates a new TaskReply instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskReply instance
   */
  public static create(properties?: ITaskReply): TaskReply;

  /**
   * Encodes the specified TaskReply message. Does not implicitly {@link TaskReply.verify|verify} messages.
   * @param message TaskReply message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskReply, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskReply message, length delimited. Does not implicitly {@link TaskReply.verify|verify} messages.
   * @param message TaskReply message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskReply, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskReply message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskReply
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskReply;

  /**
   * Decodes a TaskReply message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskReply
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskReply;

  /**
   * Verifies a TaskReply message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskReply message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskReply
   */
  public static fromObject(object: { [k: string]: any }): TaskReply;

  /**
   * Creates a plain object from a TaskReply message. Also converts values to other types if specified.
   * @param message TaskReply
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: TaskReply, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this TaskReply to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

export namespace TaskReply {
  /** Properties of a Rename. */
  interface IRename {
    /** Rename newName */
    newName?: string | null;
  }

  /** Represents a Rename. */
  class Rename implements IRename {
    /**
     * Constructs a new Rename.
     * @param [properties] Properties to set
     */
    constructor(properties?: TaskReply.IRename);

    /** Rename newName. */
    public newName: string;

    /**
     * Creates a new Rename instance using the specified properties.
     * @param [properties] Properties to set
     * @returns Rename instance
     */
    public static create(properties?: TaskReply.IRename): TaskReply.Rename;

    /**
     * Encodes the specified Rename message. Does not implicitly {@link TaskReply.Rename.verify|verify} messages.
     * @param message Rename message or plain object to encode
     * @param [writer] Writer to encode to
     * @returns Writer
     */
    public static encode(message: TaskReply.IRename, writer?: $protobuf.Writer): $protobuf.Writer;

    /**
     * Encodes the specified Rename message, length delimited. Does not implicitly {@link TaskReply.Rename.verify|verify} messages.
     * @param message Rename message or plain object to encode
     * @param [writer] Writer to encode to
     * @returns Writer
     */
    public static encodeDelimited(message: TaskReply.IRename, writer?: $protobuf.Writer): $protobuf.Writer;

    /**
     * Decodes a Rename message from the specified reader or buffer.
     * @param reader Reader or buffer to decode from
     * @param [length] Message length if known beforehand
     * @returns Rename
     * @throws {Error} If the payload is not a reader or valid buffer
     * @throws {$protobuf.util.ProtocolError} If required fields are missing
     */
    public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskReply.Rename;

    /**
     * Decodes a Rename message from the specified reader or buffer, length delimited.
     * @param reader Reader or buffer to decode from
     * @returns Rename
     * @throws {Error} If the payload is not a reader or valid buffer
     * @throws {$protobuf.util.ProtocolError} If required fields are missing
     */
    public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskReply.Rename;

    /**
     * Verifies a Rename message.
     * @param message Plain object to verify
     * @returns `null` if valid, otherwise the reason why it is not
     */
    public static verify(message: { [k: string]: any }): string | null;

    /**
     * Creates a Rename message from a plain object. Also converts values to their respective internal types.
     * @param object Plain object
     * @returns Rename
     */
    public static fromObject(object: { [k: string]: any }): TaskReply.Rename;

    /**
     * Creates a plain object from a Rename message. Also converts values to other types if specified.
     * @param message Rename
     * @param [options] Conversion options
     * @returns Plain object
     */
    public static toObject(message: TaskReply.Rename, options?: $protobuf.IConversionOptions): { [k: string]: any };

    /**
     * Converts this Rename to JSON.
     * @returns JSON object
     */
    public toJSON(): { [k: string]: any };
  }
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

/** Represents a TaskSendReplyRequest. */
export class TaskSendReplyRequest implements ITaskSendReplyRequest {
  /**
   * Constructs a new TaskSendReplyRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskSendReplyRequest);

  /** TaskSendReplyRequest reply. */
  public reply?: ITaskReply | null;

  /**
   * Creates a new TaskSendReplyRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskSendReplyRequest instance
   */
  public static create(properties?: ITaskSendReplyRequest): TaskSendReplyRequest;

  /**
   * Encodes the specified TaskSendReplyRequest message. Does not implicitly {@link TaskSendReplyRequest.verify|verify} messages.
   * @param message TaskSendReplyRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskSendReplyRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskSendReplyRequest message, length delimited. Does not implicitly {@link TaskSendReplyRequest.verify|verify} messages.
   * @param message TaskSendReplyRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskSendReplyRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskSendReplyRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskSendReplyRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskSendReplyRequest;

  /**
   * Decodes a TaskSendReplyRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskSendReplyRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskSendReplyRequest;

  /**
   * Verifies a TaskSendReplyRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskSendReplyRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskSendReplyRequest
   */
  public static fromObject(object: { [k: string]: any }): TaskSendReplyRequest;

  /**
   * Creates a plain object from a TaskSendReplyRequest message. Also converts values to other types if specified.
   * @param message TaskSendReplyRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: TaskSendReplyRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this TaskSendReplyRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Represents a TaskSendReplyResponse. */
export class TaskSendReplyResponse implements ITaskSendReplyResponse {
  /**
   * Constructs a new TaskSendReplyResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: ITaskSendReplyResponse);

  /**
   * Creates a new TaskSendReplyResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns TaskSendReplyResponse instance
   */
  public static create(properties?: ITaskSendReplyResponse): TaskSendReplyResponse;

  /**
   * Encodes the specified TaskSendReplyResponse message. Does not implicitly {@link TaskSendReplyResponse.verify|verify} messages.
   * @param message TaskSendReplyResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ITaskSendReplyResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified TaskSendReplyResponse message, length delimited. Does not implicitly {@link TaskSendReplyResponse.verify|verify} messages.
   * @param message TaskSendReplyResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ITaskSendReplyResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a TaskSendReplyResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns TaskSendReplyResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): TaskSendReplyResponse;

  /**
   * Decodes a TaskSendReplyResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns TaskSendReplyResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): TaskSendReplyResponse;

  /**
   * Verifies a TaskSendReplyResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a TaskSendReplyResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns TaskSendReplyResponse
   */
  public static fromObject(object: { [k: string]: any }): TaskSendReplyResponse;

  /**
   * Creates a plain object from a TaskSendReplyResponse message. Also converts values to other types if specified.
   * @param message TaskSendReplyResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: TaskSendReplyResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this TaskSendReplyResponse to JSON.
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
