import * as $protobuf from "protobufjs";
/** Properties of a Capability. */
export interface ICapability {
  /** Capability writable */
  writable?: boolean | null;

  /** Capability readable */
  readable?: boolean | null;

  /** Capability executable */
  executable?: boolean | null;
}

/** Represents a Capability. */
export class Capability implements ICapability {
  /**
   * Constructs a new Capability.
   * @param [properties] Properties to set
   */
  constructor(properties?: ICapability);

  /** Capability writable. */
  public writable: boolean;

  /** Capability readable. */
  public readable: boolean;

  /** Capability executable. */
  public executable: boolean;

  /**
   * Creates a new Capability instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Capability instance
   */
  public static create(properties?: ICapability): Capability;

  /**
   * Encodes the specified Capability message. Does not implicitly {@link Capability.verify|verify} messages.
   * @param message Capability message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ICapability, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Capability message, length delimited. Does not implicitly {@link Capability.verify|verify} messages.
   * @param message Capability message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ICapability, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Capability message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Capability
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Capability;

  /**
   * Decodes a Capability message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Capability
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Capability;

  /**
   * Verifies a Capability message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Capability message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Capability
   */
  public static fromObject(object: { [k: string]: any }): Capability;

  /**
   * Creates a plain object from a Capability message. Also converts values to other types if specified.
   * @param message Capability
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Capability, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Capability to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a Mode. */
export interface IMode {
  /** Mode owner */
  owner?: ICapability | null;

  /** Mode group */
  group?: ICapability | null;

  /** Mode others */
  others?: ICapability | null;
}

/** Represents a Mode. */
export class Mode implements IMode {
  /**
   * Constructs a new Mode.
   * @param [properties] Properties to set
   */
  constructor(properties?: IMode);

  /** Mode owner. */
  public owner?: ICapability | null;

  /** Mode group. */
  public group?: ICapability | null;

  /** Mode others. */
  public others?: ICapability | null;

  /**
   * Creates a new Mode instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Mode instance
   */
  public static create(properties?: IMode): Mode;

  /**
   * Encodes the specified Mode message. Does not implicitly {@link Mode.verify|verify} messages.
   * @param message Mode message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IMode, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Mode message, length delimited. Does not implicitly {@link Mode.verify|verify} messages.
   * @param message Mode message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IMode, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Mode message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Mode
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Mode;

  /**
   * Decodes a Mode message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Mode
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Mode;

  /**
   * Verifies a Mode message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Mode message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Mode
   */
  public static fromObject(object: { [k: string]: any }): Mode;

  /**
   * Creates a plain object from a Mode message. Also converts values to other types if specified.
   * @param message Mode
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Mode, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Mode to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FileStat. */
export interface IFileStat {
  /** FileStat mode */
  mode?: IMode | null;

  /** FileStat uid */
  uid?: number | null;

  /** FileStat gid */
  gid?: number | null;

  /** FileStat atime */
  atime?: string | null;

  /** FileStat ctime */
  ctime?: string | null;

  /** FileStat mtime */
  mtime?: string | null;

  /** FileStat size */
  size?: string | null;

  /** FileStat isDirectory */
  isDirectory?: boolean | null;

  /** FileStat isFile */
  isFile?: boolean | null;

  /** FileStat isSymlink */
  isSymlink?: boolean | null;
}

/** Represents a FileStat. */
export class FileStat implements IFileStat {
  /**
   * Constructs a new FileStat.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFileStat);

  /** FileStat mode. */
  public mode?: IMode | null;

  /** FileStat uid. */
  public uid: number;

  /** FileStat gid. */
  public gid: number;

  /** FileStat atime. */
  public atime: string;

  /** FileStat ctime. */
  public ctime: string;

  /** FileStat mtime. */
  public mtime: string;

  /** FileStat size. */
  public size: string;

  /** FileStat isDirectory. */
  public isDirectory: boolean;

  /** FileStat isFile. */
  public isFile: boolean;

  /** FileStat isSymlink. */
  public isSymlink: boolean;

  /**
   * Creates a new FileStat instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FileStat instance
   */
  public static create(properties?: IFileStat): FileStat;

  /**
   * Encodes the specified FileStat message. Does not implicitly {@link FileStat.verify|verify} messages.
   * @param message FileStat message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFileStat, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FileStat message, length delimited. Does not implicitly {@link FileStat.verify|verify} messages.
   * @param message FileStat message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFileStat, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FileStat message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FileStat
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FileStat;

  /**
   * Decodes a FileStat message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FileStat
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FileStat;

  /**
   * Verifies a FileStat message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FileStat message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FileStat
   */
  public static fromObject(object: { [k: string]: any }): FileStat;

  /**
   * Creates a plain object from a FileStat message. Also converts values to other types if specified.
   * @param message FileStat
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FileStat, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FileStat to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FileItem. */
export interface IFileItem {
  /** FileItem id */
  id?: string | null;

  /** FileItem parent */
  parent?: string | null;

  /** FileItem name */
  name?: string | null;

  /** FileItem fullPath */
  fullPath?: string | null;

  /** FileItem stat */
  stat?: IFileStat | null;

  /** FileItem hasLinkPath */
  hasLinkPath?: boolean | null;

  /** FileItem linkPath */
  linkPath?: string | null;
}

/** Represents a FileItem. */
export class FileItem implements IFileItem {
  /**
   * Constructs a new FileItem.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFileItem);

  /** FileItem id. */
  public id: string;

  /** FileItem parent. */
  public parent: string;

  /** FileItem name. */
  public name: string;

  /** FileItem fullPath. */
  public fullPath: string;

  /** FileItem stat. */
  public stat?: IFileStat | null;

  /** FileItem hasLinkPath. */
  public hasLinkPath: boolean;

  /** FileItem linkPath. */
  public linkPath: string;

  /**
   * Creates a new FileItem instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FileItem instance
   */
  public static create(properties?: IFileItem): FileItem;

  /**
   * Encodes the specified FileItem message. Does not implicitly {@link FileItem.verify|verify} messages.
   * @param message FileItem message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFileItem, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FileItem message, length delimited. Does not implicitly {@link FileItem.verify|verify} messages.
   * @param message FileItem message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFileItem, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FileItem message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FileItem
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FileItem;

  /**
   * Decodes a FileItem message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FileItem
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FileItem;

  /**
   * Verifies a FileItem message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FileItem message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FileItem
   */
  public static fromObject(object: { [k: string]: any }): FileItem;

  /**
   * Creates a plain object from a FileItem message. Also converts values to other types if specified.
   * @param message FileItem
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FileItem, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FileItem to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FileList. */
export interface IFileList {
  /** FileList location */
  location?: string | null;

  /** FileList items */
  items?: IFileItem[] | null;
}

/** Represents a FileList. */
export class FileList implements IFileList {
  /**
   * Constructs a new FileList.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFileList);

  /** FileList location. */
  public location: string;

  /** FileList items. */
  public items: IFileItem[];

  /**
   * Creates a new FileList instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FileList instance
   */
  public static create(properties?: IFileList): FileList;

  /**
   * Encodes the specified FileList message. Does not implicitly {@link FileList.verify|verify} messages.
   * @param message FileList message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFileList, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FileList message, length delimited. Does not implicitly {@link FileList.verify|verify} messages.
   * @param message FileList message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFileList, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FileList message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FileList
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FileList;

  /**
   * Decodes a FileList message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FileList
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FileList;

  /**
   * Verifies a FileList message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FileList message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FileList
   */
  public static fromObject(object: { [k: string]: any }): FileList;

  /**
   * Creates a plain object from a FileList message. Also converts values to other types if specified.
   * @param message FileList
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FileList, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FileList to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a LocationHistory. */
export interface ILocationHistory {
  /** LocationHistory records */
  records?: ILocationRecord[] | null;

  /** LocationHistory maxRecordNumber */
  maxRecordNumber?: number | null;
}

/** Represents a LocationHistory. */
export class LocationHistory implements ILocationHistory {
  /**
   * Constructs a new LocationHistory.
   * @param [properties] Properties to set
   */
  constructor(properties?: ILocationHistory);

  /** LocationHistory records. */
  public records: ILocationRecord[];

  /** LocationHistory maxRecordNumber. */
  public maxRecordNumber: number;

  /**
   * Creates a new LocationHistory instance using the specified properties.
   * @param [properties] Properties to set
   * @returns LocationHistory instance
   */
  public static create(properties?: ILocationHistory): LocationHistory;

  /**
   * Encodes the specified LocationHistory message. Does not implicitly {@link LocationHistory.verify|verify} messages.
   * @param message LocationHistory message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ILocationHistory, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified LocationHistory message, length delimited. Does not implicitly {@link LocationHistory.verify|verify} messages.
   * @param message LocationHistory message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ILocationHistory, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a LocationHistory message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns LocationHistory
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): LocationHistory;

  /**
   * Decodes a LocationHistory message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns LocationHistory
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): LocationHistory;

  /**
   * Verifies a LocationHistory message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a LocationHistory message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns LocationHistory
   */
  public static fromObject(object: { [k: string]: any }): LocationHistory;

  /**
   * Creates a plain object from a LocationHistory message. Also converts values to other types if specified.
   * @param message LocationHistory
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: LocationHistory, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this LocationHistory to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a LocationRecord. */
export interface ILocationRecord {
  /** LocationRecord location */
  location?: string | null;

  /** LocationRecord timestamp */
  timestamp?: string | null;
}

/** Represents a LocationRecord. */
export class LocationRecord implements ILocationRecord {
  /**
   * Constructs a new LocationRecord.
   * @param [properties] Properties to set
   */
  constructor(properties?: ILocationRecord);

  /** LocationRecord location. */
  public location: string;

  /** LocationRecord timestamp. */
  public timestamp: string;

  /**
   * Creates a new LocationRecord instance using the specified properties.
   * @param [properties] Properties to set
   * @returns LocationRecord instance
   */
  public static create(properties?: ILocationRecord): LocationRecord;

  /**
   * Encodes the specified LocationRecord message. Does not implicitly {@link LocationRecord.verify|verify} messages.
   * @param message LocationRecord message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: ILocationRecord, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified LocationRecord message, length delimited. Does not implicitly {@link LocationRecord.verify|verify} messages.
   * @param message LocationRecord message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: ILocationRecord, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a LocationRecord message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns LocationRecord
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): LocationRecord;

  /**
   * Decodes a LocationRecord message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns LocationRecord
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): LocationRecord;

  /**
   * Verifies a LocationRecord message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a LocationRecord message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns LocationRecord
   */
  public static fromObject(object: { [k: string]: any }): LocationRecord;

  /**
   * Creates a plain object from a LocationRecord message. Also converts values to other types if specified.
   * @param message LocationRecord
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: LocationRecord, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this LocationRecord to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a Filer. */
export interface IFiler {
  /** Filer id */
  id?: string | null;

  /** Filer name */
  name?: string | null;

  /** Filer fileList */
  fileList?: IFileList | null;

  /** Filer history */
  history?: ILocationHistory | null;

  /** Filer markedItems */
  markedItems?: string[] | null;

  /** Filer sortOrder */
  sortOrder?: SortType | null;
}

/** Represents a Filer. */
export class Filer implements IFiler {
  /**
   * Constructs a new Filer.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFiler);

  /** Filer id. */
  public id: string;

  /** Filer name. */
  public name: string;

  /** Filer fileList. */
  public fileList?: IFileList | null;

  /** Filer history. */
  public history?: ILocationHistory | null;

  /** Filer markedItems. */
  public markedItems: string[];

  /** Filer sortOrder. */
  public sortOrder: SortType;

  /**
   * Creates a new Filer instance using the specified properties.
   * @param [properties] Properties to set
   * @returns Filer instance
   */
  public static create(properties?: IFiler): Filer;

  /**
   * Encodes the specified Filer message. Does not implicitly {@link Filer.verify|verify} messages.
   * @param message Filer message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFiler, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified Filer message, length delimited. Does not implicitly {@link Filer.verify|verify} messages.
   * @param message Filer message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFiler, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a Filer message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns Filer
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): Filer;

  /**
   * Decodes a Filer message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns Filer
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): Filer;

  /**
   * Verifies a Filer message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a Filer message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns Filer
   */
  public static fromObject(object: { [k: string]: any }): Filer;

  /**
   * Creates a plain object from a Filer message. Also converts values to other types if specified.
   * @param message Filer
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: Filer, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this Filer to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerMakeRequest. */
export interface IFilerMakeRequest {
  /** FilerMakeRequest initialLocation */
  initialLocation?: string | null;

  /** FilerMakeRequest name */
  name?: string | null;
}

/** Represents a FilerMakeRequest. */
export class FilerMakeRequest implements IFilerMakeRequest {
  /**
   * Constructs a new FilerMakeRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerMakeRequest);

  /** FilerMakeRequest initialLocation. */
  public initialLocation: string;

  /** FilerMakeRequest name. */
  public name: string;

  /**
   * Creates a new FilerMakeRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerMakeRequest instance
   */
  public static create(properties?: IFilerMakeRequest): FilerMakeRequest;

  /**
   * Encodes the specified FilerMakeRequest message. Does not implicitly {@link FilerMakeRequest.verify|verify} messages.
   * @param message FilerMakeRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerMakeRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerMakeRequest message, length delimited. Does not implicitly {@link FilerMakeRequest.verify|verify} messages.
   * @param message FilerMakeRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerMakeRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerMakeRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerMakeRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerMakeRequest;

  /**
   * Decodes a FilerMakeRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerMakeRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerMakeRequest;

  /**
   * Verifies a FilerMakeRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerMakeRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerMakeRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerMakeRequest;

  /**
   * Creates a plain object from a FilerMakeRequest message. Also converts values to other types if specified.
   * @param message FilerMakeRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerMakeRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerMakeRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerMakeResponse. */
export interface IFilerMakeResponse {
  /** FilerMakeResponse filer */
  filer?: IFiler | null;
}

/** Represents a FilerMakeResponse. */
export class FilerMakeResponse implements IFilerMakeResponse {
  /**
   * Constructs a new FilerMakeResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerMakeResponse);

  /** FilerMakeResponse filer. */
  public filer?: IFiler | null;

  /**
   * Creates a new FilerMakeResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerMakeResponse instance
   */
  public static create(properties?: IFilerMakeResponse): FilerMakeResponse;

  /**
   * Encodes the specified FilerMakeResponse message. Does not implicitly {@link FilerMakeResponse.verify|verify} messages.
   * @param message FilerMakeResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerMakeResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerMakeResponse message, length delimited. Does not implicitly {@link FilerMakeResponse.verify|verify} messages.
   * @param message FilerMakeResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerMakeResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerMakeResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerMakeResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerMakeResponse;

  /**
   * Decodes a FilerMakeResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerMakeResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerMakeResponse;

  /**
   * Verifies a FilerMakeResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerMakeResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerMakeResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerMakeResponse;

  /**
   * Creates a plain object from a FilerMakeResponse message. Also converts values to other types if specified.
   * @param message FilerMakeResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerMakeResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerMakeResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerGetRequest. */
export interface IFilerGetRequest {
  /** FilerGetRequest name */
  name?: string | null;
}

/** Represents a FilerGetRequest. */
export class FilerGetRequest implements IFilerGetRequest {
  /**
   * Constructs a new FilerGetRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerGetRequest);

  /** FilerGetRequest name. */
  public name: string;

  /**
   * Creates a new FilerGetRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerGetRequest instance
   */
  public static create(properties?: IFilerGetRequest): FilerGetRequest;

  /**
   * Encodes the specified FilerGetRequest message. Does not implicitly {@link FilerGetRequest.verify|verify} messages.
   * @param message FilerGetRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerGetRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerGetRequest message, length delimited. Does not implicitly {@link FilerGetRequest.verify|verify} messages.
   * @param message FilerGetRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerGetRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerGetRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerGetRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerGetRequest;

  /**
   * Decodes a FilerGetRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerGetRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerGetRequest;

  /**
   * Verifies a FilerGetRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerGetRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerGetRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerGetRequest;

  /**
   * Creates a plain object from a FilerGetRequest message. Also converts values to other types if specified.
   * @param message FilerGetRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerGetRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerGetRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerGetResponse. */
export interface IFilerGetResponse {
  /** FilerGetResponse filer */
  filer?: IFiler | null;
}

/** Represents a FilerGetResponse. */
export class FilerGetResponse implements IFilerGetResponse {
  /**
   * Constructs a new FilerGetResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerGetResponse);

  /** FilerGetResponse filer. */
  public filer?: IFiler | null;

  /**
   * Creates a new FilerGetResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerGetResponse instance
   */
  public static create(properties?: IFilerGetResponse): FilerGetResponse;

  /**
   * Encodes the specified FilerGetResponse message. Does not implicitly {@link FilerGetResponse.verify|verify} messages.
   * @param message FilerGetResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerGetResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerGetResponse message, length delimited. Does not implicitly {@link FilerGetResponse.verify|verify} messages.
   * @param message FilerGetResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerGetResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerGetResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerGetResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerGetResponse;

  /**
   * Decodes a FilerGetResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerGetResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerGetResponse;

  /**
   * Verifies a FilerGetResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerGetResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerGetResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerGetResponse;

  /**
   * Creates a plain object from a FilerGetResponse message. Also converts values to other types if specified.
   * @param message FilerGetResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerGetResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerGetResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerMoveParentRequest. */
export interface IFilerMoveParentRequest {
  /** FilerMoveParentRequest name */
  name?: string | null;
}

/** Represents a FilerMoveParentRequest. */
export class FilerMoveParentRequest implements IFilerMoveParentRequest {
  /**
   * Constructs a new FilerMoveParentRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerMoveParentRequest);

  /** FilerMoveParentRequest name. */
  public name: string;

  /**
   * Creates a new FilerMoveParentRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerMoveParentRequest instance
   */
  public static create(properties?: IFilerMoveParentRequest): FilerMoveParentRequest;

  /**
   * Encodes the specified FilerMoveParentRequest message. Does not implicitly {@link FilerMoveParentRequest.verify|verify} messages.
   * @param message FilerMoveParentRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerMoveParentRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerMoveParentRequest message, length delimited. Does not implicitly {@link FilerMoveParentRequest.verify|verify} messages.
   * @param message FilerMoveParentRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerMoveParentRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerMoveParentRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerMoveParentRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerMoveParentRequest;

  /**
   * Decodes a FilerMoveParentRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerMoveParentRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerMoveParentRequest;

  /**
   * Verifies a FilerMoveParentRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerMoveParentRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerMoveParentRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerMoveParentRequest;

  /**
   * Creates a plain object from a FilerMoveParentRequest message. Also converts values to other types if specified.
   * @param message FilerMoveParentRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerMoveParentRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerMoveParentRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerMoveParentResponse. */
export interface IFilerMoveParentResponse {
  /** FilerMoveParentResponse filer */
  filer?: IFiler | null;
}

/** Represents a FilerMoveParentResponse. */
export class FilerMoveParentResponse implements IFilerMoveParentResponse {
  /**
   * Constructs a new FilerMoveParentResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerMoveParentResponse);

  /** FilerMoveParentResponse filer. */
  public filer?: IFiler | null;

  /**
   * Creates a new FilerMoveParentResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerMoveParentResponse instance
   */
  public static create(properties?: IFilerMoveParentResponse): FilerMoveParentResponse;

  /**
   * Encodes the specified FilerMoveParentResponse message. Does not implicitly {@link FilerMoveParentResponse.verify|verify} messages.
   * @param message FilerMoveParentResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerMoveParentResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerMoveParentResponse message, length delimited. Does not implicitly {@link FilerMoveParentResponse.verify|verify} messages.
   * @param message FilerMoveParentResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerMoveParentResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerMoveParentResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerMoveParentResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerMoveParentResponse;

  /**
   * Decodes a FilerMoveParentResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerMoveParentResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerMoveParentResponse;

  /**
   * Verifies a FilerMoveParentResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerMoveParentResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerMoveParentResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerMoveParentResponse;

  /**
   * Creates a plain object from a FilerMoveParentResponse message. Also converts values to other types if specified.
   * @param message FilerMoveParentResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: FilerMoveParentResponse,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this FilerMoveParentResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerEnterDirectoryRequest. */
export interface IFilerEnterDirectoryRequest {
  /** FilerEnterDirectoryRequest name */
  name?: string | null;

  /** FilerEnterDirectoryRequest itemId */
  itemId?: string | null;
}

/** Represents a FilerEnterDirectoryRequest. */
export class FilerEnterDirectoryRequest implements IFilerEnterDirectoryRequest {
  /**
   * Constructs a new FilerEnterDirectoryRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerEnterDirectoryRequest);

  /** FilerEnterDirectoryRequest name. */
  public name: string;

  /** FilerEnterDirectoryRequest itemId. */
  public itemId: string;

  /**
   * Creates a new FilerEnterDirectoryRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerEnterDirectoryRequest instance
   */
  public static create(properties?: IFilerEnterDirectoryRequest): FilerEnterDirectoryRequest;

  /**
   * Encodes the specified FilerEnterDirectoryRequest message. Does not implicitly {@link FilerEnterDirectoryRequest.verify|verify} messages.
   * @param message FilerEnterDirectoryRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerEnterDirectoryRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerEnterDirectoryRequest message, length delimited. Does not implicitly {@link FilerEnterDirectoryRequest.verify|verify} messages.
   * @param message FilerEnterDirectoryRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerEnterDirectoryRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerEnterDirectoryRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerEnterDirectoryRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerEnterDirectoryRequest;

  /**
   * Decodes a FilerEnterDirectoryRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerEnterDirectoryRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerEnterDirectoryRequest;

  /**
   * Verifies a FilerEnterDirectoryRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerEnterDirectoryRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerEnterDirectoryRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerEnterDirectoryRequest;

  /**
   * Creates a plain object from a FilerEnterDirectoryRequest message. Also converts values to other types if specified.
   * @param message FilerEnterDirectoryRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: FilerEnterDirectoryRequest,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this FilerEnterDirectoryRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerEnterDirectoryResponse. */
export interface IFilerEnterDirectoryResponse {
  /** FilerEnterDirectoryResponse filer */
  filer?: IFiler | null;
}

/** Represents a FilerEnterDirectoryResponse. */
export class FilerEnterDirectoryResponse implements IFilerEnterDirectoryResponse {
  /**
   * Constructs a new FilerEnterDirectoryResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerEnterDirectoryResponse);

  /** FilerEnterDirectoryResponse filer. */
  public filer?: IFiler | null;

  /**
   * Creates a new FilerEnterDirectoryResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerEnterDirectoryResponse instance
   */
  public static create(properties?: IFilerEnterDirectoryResponse): FilerEnterDirectoryResponse;

  /**
   * Encodes the specified FilerEnterDirectoryResponse message. Does not implicitly {@link FilerEnterDirectoryResponse.verify|verify} messages.
   * @param message FilerEnterDirectoryResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerEnterDirectoryResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerEnterDirectoryResponse message, length delimited. Does not implicitly {@link FilerEnterDirectoryResponse.verify|verify} messages.
   * @param message FilerEnterDirectoryResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerEnterDirectoryResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerEnterDirectoryResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerEnterDirectoryResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerEnterDirectoryResponse;

  /**
   * Decodes a FilerEnterDirectoryResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerEnterDirectoryResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerEnterDirectoryResponse;

  /**
   * Verifies a FilerEnterDirectoryResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerEnterDirectoryResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerEnterDirectoryResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerEnterDirectoryResponse;

  /**
   * Creates a plain object from a FilerEnterDirectoryResponse message. Also converts values to other types if specified.
   * @param message FilerEnterDirectoryResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: FilerEnterDirectoryResponse,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this FilerEnterDirectoryResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerToggleMarkRequest. */
export interface IFilerToggleMarkRequest {
  /** FilerToggleMarkRequest name */
  name?: string | null;

  /** FilerToggleMarkRequest itemIds */
  itemIds?: string[] | null;
}

/** Represents a FilerToggleMarkRequest. */
export class FilerToggleMarkRequest implements IFilerToggleMarkRequest {
  /**
   * Constructs a new FilerToggleMarkRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerToggleMarkRequest);

  /** FilerToggleMarkRequest name. */
  public name: string;

  /** FilerToggleMarkRequest itemIds. */
  public itemIds: string[];

  /**
   * Creates a new FilerToggleMarkRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerToggleMarkRequest instance
   */
  public static create(properties?: IFilerToggleMarkRequest): FilerToggleMarkRequest;

  /**
   * Encodes the specified FilerToggleMarkRequest message. Does not implicitly {@link FilerToggleMarkRequest.verify|verify} messages.
   * @param message FilerToggleMarkRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerToggleMarkRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerToggleMarkRequest message, length delimited. Does not implicitly {@link FilerToggleMarkRequest.verify|verify} messages.
   * @param message FilerToggleMarkRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerToggleMarkRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerToggleMarkRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerToggleMarkRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerToggleMarkRequest;

  /**
   * Decodes a FilerToggleMarkRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerToggleMarkRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerToggleMarkRequest;

  /**
   * Verifies a FilerToggleMarkRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerToggleMarkRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerToggleMarkRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerToggleMarkRequest;

  /**
   * Creates a plain object from a FilerToggleMarkRequest message. Also converts values to other types if specified.
   * @param message FilerToggleMarkRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerToggleMarkRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerToggleMarkRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerToggleMarkResponse. */
export interface IFilerToggleMarkResponse {
  /** FilerToggleMarkResponse filer */
  filer?: IFiler | null;
}

/** Represents a FilerToggleMarkResponse. */
export class FilerToggleMarkResponse implements IFilerToggleMarkResponse {
  /**
   * Constructs a new FilerToggleMarkResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerToggleMarkResponse);

  /** FilerToggleMarkResponse filer. */
  public filer?: IFiler | null;

  /**
   * Creates a new FilerToggleMarkResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerToggleMarkResponse instance
   */
  public static create(properties?: IFilerToggleMarkResponse): FilerToggleMarkResponse;

  /**
   * Encodes the specified FilerToggleMarkResponse message. Does not implicitly {@link FilerToggleMarkResponse.verify|verify} messages.
   * @param message FilerToggleMarkResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerToggleMarkResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerToggleMarkResponse message, length delimited. Does not implicitly {@link FilerToggleMarkResponse.verify|verify} messages.
   * @param message FilerToggleMarkResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerToggleMarkResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerToggleMarkResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerToggleMarkResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerToggleMarkResponse;

  /**
   * Decodes a FilerToggleMarkResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerToggleMarkResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerToggleMarkResponse;

  /**
   * Verifies a FilerToggleMarkResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerToggleMarkResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerToggleMarkResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerToggleMarkResponse;

  /**
   * Creates a plain object from a FilerToggleMarkResponse message. Also converts values to other types if specified.
   * @param message FilerToggleMarkResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: FilerToggleMarkResponse,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this FilerToggleMarkResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerMoveRequest. */
export interface IFilerMoveRequest {
  /** FilerMoveRequest source */
  source?: string | null;

  /** FilerMoveRequest dest */
  dest?: string | null;

  /** FilerMoveRequest itemIds */
  itemIds?: string[] | null;
}

/** Represents a FilerMoveRequest. */
export class FilerMoveRequest implements IFilerMoveRequest {
  /**
   * Constructs a new FilerMoveRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerMoveRequest);

  /** FilerMoveRequest source. */
  public source: string;

  /** FilerMoveRequest dest. */
  public dest: string;

  /** FilerMoveRequest itemIds. */
  public itemIds: string[];

  /**
   * Creates a new FilerMoveRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerMoveRequest instance
   */
  public static create(properties?: IFilerMoveRequest): FilerMoveRequest;

  /**
   * Encodes the specified FilerMoveRequest message. Does not implicitly {@link FilerMoveRequest.verify|verify} messages.
   * @param message FilerMoveRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerMoveRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerMoveRequest message, length delimited. Does not implicitly {@link FilerMoveRequest.verify|verify} messages.
   * @param message FilerMoveRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerMoveRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerMoveRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerMoveRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerMoveRequest;

  /**
   * Decodes a FilerMoveRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerMoveRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerMoveRequest;

  /**
   * Verifies a FilerMoveRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerMoveRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerMoveRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerMoveRequest;

  /**
   * Creates a plain object from a FilerMoveRequest message. Also converts values to other types if specified.
   * @param message FilerMoveRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerMoveRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerMoveRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerMoveResponse. */
export interface IFilerMoveResponse {
  /** FilerMoveResponse taskId */
  taskId?: string | null;

  /** FilerMoveResponse taskName */
  taskName?: string | null;
}

/** Represents a FilerMoveResponse. */
export class FilerMoveResponse implements IFilerMoveResponse {
  /**
   * Constructs a new FilerMoveResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerMoveResponse);

  /** FilerMoveResponse taskId. */
  public taskId: string;

  /** FilerMoveResponse taskName. */
  public taskName: string;

  /**
   * Creates a new FilerMoveResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerMoveResponse instance
   */
  public static create(properties?: IFilerMoveResponse): FilerMoveResponse;

  /**
   * Encodes the specified FilerMoveResponse message. Does not implicitly {@link FilerMoveResponse.verify|verify} messages.
   * @param message FilerMoveResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerMoveResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerMoveResponse message, length delimited. Does not implicitly {@link FilerMoveResponse.verify|verify} messages.
   * @param message FilerMoveResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerMoveResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerMoveResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerMoveResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerMoveResponse;

  /**
   * Decodes a FilerMoveResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerMoveResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerMoveResponse;

  /**
   * Verifies a FilerMoveResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerMoveResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerMoveResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerMoveResponse;

  /**
   * Creates a plain object from a FilerMoveResponse message. Also converts values to other types if specified.
   * @param message FilerMoveResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerMoveResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerMoveResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerDeleteRequest. */
export interface IFilerDeleteRequest {
  /** FilerDeleteRequest source */
  source?: string | null;

  /** FilerDeleteRequest itemIds */
  itemIds?: string[] | null;
}

/** Represents a FilerDeleteRequest. */
export class FilerDeleteRequest implements IFilerDeleteRequest {
  /**
   * Constructs a new FilerDeleteRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerDeleteRequest);

  /** FilerDeleteRequest source. */
  public source: string;

  /** FilerDeleteRequest itemIds. */
  public itemIds: string[];

  /**
   * Creates a new FilerDeleteRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerDeleteRequest instance
   */
  public static create(properties?: IFilerDeleteRequest): FilerDeleteRequest;

  /**
   * Encodes the specified FilerDeleteRequest message. Does not implicitly {@link FilerDeleteRequest.verify|verify} messages.
   * @param message FilerDeleteRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerDeleteRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerDeleteRequest message, length delimited. Does not implicitly {@link FilerDeleteRequest.verify|verify} messages.
   * @param message FilerDeleteRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerDeleteRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerDeleteRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerDeleteRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerDeleteRequest;

  /**
   * Decodes a FilerDeleteRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerDeleteRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerDeleteRequest;

  /**
   * Verifies a FilerDeleteRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerDeleteRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerDeleteRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerDeleteRequest;

  /**
   * Creates a plain object from a FilerDeleteRequest message. Also converts values to other types if specified.
   * @param message FilerDeleteRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerDeleteRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerDeleteRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerDeleteResponse. */
export interface IFilerDeleteResponse {
  /** FilerDeleteResponse taskId */
  taskId?: string | null;

  /** FilerDeleteResponse taskName */
  taskName?: string | null;
}

/** Represents a FilerDeleteResponse. */
export class FilerDeleteResponse implements IFilerDeleteResponse {
  /**
   * Constructs a new FilerDeleteResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerDeleteResponse);

  /** FilerDeleteResponse taskId. */
  public taskId: string;

  /** FilerDeleteResponse taskName. */
  public taskName: string;

  /**
   * Creates a new FilerDeleteResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerDeleteResponse instance
   */
  public static create(properties?: IFilerDeleteResponse): FilerDeleteResponse;

  /**
   * Encodes the specified FilerDeleteResponse message. Does not implicitly {@link FilerDeleteResponse.verify|verify} messages.
   * @param message FilerDeleteResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerDeleteResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerDeleteResponse message, length delimited. Does not implicitly {@link FilerDeleteResponse.verify|verify} messages.
   * @param message FilerDeleteResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerDeleteResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerDeleteResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerDeleteResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerDeleteResponse;

  /**
   * Decodes a FilerDeleteResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerDeleteResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerDeleteResponse;

  /**
   * Verifies a FilerDeleteResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerDeleteResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerDeleteResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerDeleteResponse;

  /**
   * Creates a plain object from a FilerDeleteResponse message. Also converts values to other types if specified.
   * @param message FilerDeleteResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerDeleteResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerDeleteResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerCopyRequest. */
export interface IFilerCopyRequest {
  /** FilerCopyRequest source */
  source?: string | null;

  /** FilerCopyRequest dest */
  dest?: string | null;

  /** FilerCopyRequest itemIds */
  itemIds?: string[] | null;
}

/** Represents a FilerCopyRequest. */
export class FilerCopyRequest implements IFilerCopyRequest {
  /**
   * Constructs a new FilerCopyRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerCopyRequest);

  /** FilerCopyRequest source. */
  public source: string;

  /** FilerCopyRequest dest. */
  public dest: string;

  /** FilerCopyRequest itemIds. */
  public itemIds: string[];

  /**
   * Creates a new FilerCopyRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerCopyRequest instance
   */
  public static create(properties?: IFilerCopyRequest): FilerCopyRequest;

  /**
   * Encodes the specified FilerCopyRequest message. Does not implicitly {@link FilerCopyRequest.verify|verify} messages.
   * @param message FilerCopyRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerCopyRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerCopyRequest message, length delimited. Does not implicitly {@link FilerCopyRequest.verify|verify} messages.
   * @param message FilerCopyRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerCopyRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerCopyRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerCopyRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerCopyRequest;

  /**
   * Decodes a FilerCopyRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerCopyRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerCopyRequest;

  /**
   * Verifies a FilerCopyRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerCopyRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerCopyRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerCopyRequest;

  /**
   * Creates a plain object from a FilerCopyRequest message. Also converts values to other types if specified.
   * @param message FilerCopyRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerCopyRequest, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerCopyRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerCopyResponse. */
export interface IFilerCopyResponse {
  /** FilerCopyResponse taskId */
  taskId?: string | null;

  /** FilerCopyResponse taskName */
  taskName?: string | null;
}

/** Represents a FilerCopyResponse. */
export class FilerCopyResponse implements IFilerCopyResponse {
  /**
   * Constructs a new FilerCopyResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerCopyResponse);

  /** FilerCopyResponse taskId. */
  public taskId: string;

  /** FilerCopyResponse taskName. */
  public taskName: string;

  /**
   * Creates a new FilerCopyResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerCopyResponse instance
   */
  public static create(properties?: IFilerCopyResponse): FilerCopyResponse;

  /**
   * Encodes the specified FilerCopyResponse message. Does not implicitly {@link FilerCopyResponse.verify|verify} messages.
   * @param message FilerCopyResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerCopyResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerCopyResponse message, length delimited. Does not implicitly {@link FilerCopyResponse.verify|verify} messages.
   * @param message FilerCopyResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerCopyResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerCopyResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerCopyResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerCopyResponse;

  /**
   * Decodes a FilerCopyResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerCopyResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerCopyResponse;

  /**
   * Verifies a FilerCopyResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerCopyResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerCopyResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerCopyResponse;

  /**
   * Creates a plain object from a FilerCopyResponse message. Also converts values to other types if specified.
   * @param message FilerCopyResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(message: FilerCopyResponse, options?: $protobuf.IConversionOptions): { [k: string]: any };

  /**
   * Converts this FilerCopyResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerJumpLocationRequest. */
export interface IFilerJumpLocationRequest {
  /** FilerJumpLocationRequest location */
  location?: string | null;

  /** FilerJumpLocationRequest name */
  name?: string | null;
}

/** Represents a FilerJumpLocationRequest. */
export class FilerJumpLocationRequest implements IFilerJumpLocationRequest {
  /**
   * Constructs a new FilerJumpLocationRequest.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerJumpLocationRequest);

  /** FilerJumpLocationRequest location. */
  public location: string;

  /** FilerJumpLocationRequest name. */
  public name: string;

  /**
   * Creates a new FilerJumpLocationRequest instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerJumpLocationRequest instance
   */
  public static create(properties?: IFilerJumpLocationRequest): FilerJumpLocationRequest;

  /**
   * Encodes the specified FilerJumpLocationRequest message. Does not implicitly {@link FilerJumpLocationRequest.verify|verify} messages.
   * @param message FilerJumpLocationRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerJumpLocationRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerJumpLocationRequest message, length delimited. Does not implicitly {@link FilerJumpLocationRequest.verify|verify} messages.
   * @param message FilerJumpLocationRequest message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerJumpLocationRequest, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerJumpLocationRequest message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerJumpLocationRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerJumpLocationRequest;

  /**
   * Decodes a FilerJumpLocationRequest message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerJumpLocationRequest
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerJumpLocationRequest;

  /**
   * Verifies a FilerJumpLocationRequest message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerJumpLocationRequest message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerJumpLocationRequest
   */
  public static fromObject(object: { [k: string]: any }): FilerJumpLocationRequest;

  /**
   * Creates a plain object from a FilerJumpLocationRequest message. Also converts values to other types if specified.
   * @param message FilerJumpLocationRequest
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: FilerJumpLocationRequest,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this FilerJumpLocationRequest to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}

/** Properties of a FilerJumpLocationResponse. */
export interface IFilerJumpLocationResponse {
  /** FilerJumpLocationResponse filer */
  filer?: IFiler | null;
}

/** Represents a FilerJumpLocationResponse. */
export class FilerJumpLocationResponse implements IFilerJumpLocationResponse {
  /**
   * Constructs a new FilerJumpLocationResponse.
   * @param [properties] Properties to set
   */
  constructor(properties?: IFilerJumpLocationResponse);

  /** FilerJumpLocationResponse filer. */
  public filer?: IFiler | null;

  /**
   * Creates a new FilerJumpLocationResponse instance using the specified properties.
   * @param [properties] Properties to set
   * @returns FilerJumpLocationResponse instance
   */
  public static create(properties?: IFilerJumpLocationResponse): FilerJumpLocationResponse;

  /**
   * Encodes the specified FilerJumpLocationResponse message. Does not implicitly {@link FilerJumpLocationResponse.verify|verify} messages.
   * @param message FilerJumpLocationResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encode(message: IFilerJumpLocationResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Encodes the specified FilerJumpLocationResponse message, length delimited. Does not implicitly {@link FilerJumpLocationResponse.verify|verify} messages.
   * @param message FilerJumpLocationResponse message or plain object to encode
   * @param [writer] Writer to encode to
   * @returns Writer
   */
  public static encodeDelimited(message: IFilerJumpLocationResponse, writer?: $protobuf.Writer): $protobuf.Writer;

  /**
   * Decodes a FilerJumpLocationResponse message from the specified reader or buffer.
   * @param reader Reader or buffer to decode from
   * @param [length] Message length if known beforehand
   * @returns FilerJumpLocationResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decode(reader: $protobuf.Reader | Uint8Array, length?: number): FilerJumpLocationResponse;

  /**
   * Decodes a FilerJumpLocationResponse message from the specified reader or buffer, length delimited.
   * @param reader Reader or buffer to decode from
   * @returns FilerJumpLocationResponse
   * @throws {Error} If the payload is not a reader or valid buffer
   * @throws {$protobuf.util.ProtocolError} If required fields are missing
   */
  public static decodeDelimited(reader: $protobuf.Reader | Uint8Array): FilerJumpLocationResponse;

  /**
   * Verifies a FilerJumpLocationResponse message.
   * @param message Plain object to verify
   * @returns `null` if valid, otherwise the reason why it is not
   */
  public static verify(message: { [k: string]: any }): string | null;

  /**
   * Creates a FilerJumpLocationResponse message from a plain object. Also converts values to their respective internal types.
   * @param object Plain object
   * @returns FilerJumpLocationResponse
   */
  public static fromObject(object: { [k: string]: any }): FilerJumpLocationResponse;

  /**
   * Creates a plain object from a FilerJumpLocationResponse message. Also converts values to other types if specified.
   * @param message FilerJumpLocationResponse
   * @param [options] Conversion options
   * @returns Plain object
   */
  public static toObject(
    message: FilerJumpLocationResponse,
    options?: $protobuf.IConversionOptions
  ): { [k: string]: any };

  /**
   * Converts this FilerJumpLocationResponse to JSON.
   * @returns JSON object
   */
  public toJSON(): { [k: string]: any };
}
