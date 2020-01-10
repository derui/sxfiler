/*eslint-disable block-scoped-var, id-length, no-control-regex, no-magic-numbers, no-prototype-builtins, no-redeclare, no-shadow, no-var, sort-vars*/
(function(global, factory) { /* global define, require, module */

    /* AMD */ if (typeof define === 'function' && define.amd)
        define(["protobufjs/minimal"], factory);

    /* CommonJS */ else if (typeof require === 'function' && typeof module === 'object' && module && module.exports)
        module.exports = factory(require("protobufjs/minimal"));

})(this, function($protobuf) {
    "use strict";

    // Common aliases
    var $Reader = $protobuf.Reader, $Writer = $protobuf.Writer, $util = $protobuf.util;
    
    // Exported root namespace
    var $root = $protobuf.roots["default"] || ($protobuf.roots["default"] = {});
    
    $root.Capability = (function() {
    
        /**
         * Properties of a Capability.
         * @exports ICapability
         * @interface ICapability
         * @property {boolean|null} [writable] Capability writable
         * @property {boolean|null} [readable] Capability readable
         * @property {boolean|null} [executable] Capability executable
         */
    
        /**
         * Constructs a new Capability.
         * @exports Capability
         * @classdesc Represents a Capability.
         * @implements ICapability
         * @constructor
         * @param {ICapability=} [properties] Properties to set
         */
        function Capability(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Capability writable.
         * @member {boolean} writable
         * @memberof Capability
         * @instance
         */
        Capability.prototype.writable = false;
    
        /**
         * Capability readable.
         * @member {boolean} readable
         * @memberof Capability
         * @instance
         */
        Capability.prototype.readable = false;
    
        /**
         * Capability executable.
         * @member {boolean} executable
         * @memberof Capability
         * @instance
         */
        Capability.prototype.executable = false;
    
        /**
         * Creates a new Capability instance using the specified properties.
         * @function create
         * @memberof Capability
         * @static
         * @param {ICapability=} [properties] Properties to set
         * @returns {Capability} Capability instance
         */
        Capability.create = function create(properties) {
            return new Capability(properties);
        };
    
        /**
         * Encodes the specified Capability message. Does not implicitly {@link Capability.verify|verify} messages.
         * @function encode
         * @memberof Capability
         * @static
         * @param {ICapability} message Capability message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Capability.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.writable != null && message.hasOwnProperty("writable"))
                writer.uint32(/* id 1, wireType 0 =*/8).bool(message.writable);
            if (message.readable != null && message.hasOwnProperty("readable"))
                writer.uint32(/* id 2, wireType 0 =*/16).bool(message.readable);
            if (message.executable != null && message.hasOwnProperty("executable"))
                writer.uint32(/* id 3, wireType 0 =*/24).bool(message.executable);
            return writer;
        };
    
        /**
         * Encodes the specified Capability message, length delimited. Does not implicitly {@link Capability.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Capability
         * @static
         * @param {ICapability} message Capability message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Capability.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Capability message from the specified reader or buffer.
         * @function decode
         * @memberof Capability
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Capability} Capability
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Capability.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Capability();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.writable = reader.bool();
                    break;
                case 2:
                    message.readable = reader.bool();
                    break;
                case 3:
                    message.executable = reader.bool();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Capability message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Capability
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Capability} Capability
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Capability.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Capability message.
         * @function verify
         * @memberof Capability
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Capability.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.writable != null && message.hasOwnProperty("writable"))
                if (typeof message.writable !== "boolean")
                    return "writable: boolean expected";
            if (message.readable != null && message.hasOwnProperty("readable"))
                if (typeof message.readable !== "boolean")
                    return "readable: boolean expected";
            if (message.executable != null && message.hasOwnProperty("executable"))
                if (typeof message.executable !== "boolean")
                    return "executable: boolean expected";
            return null;
        };
    
        /**
         * Creates a Capability message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Capability
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Capability} Capability
         */
        Capability.fromObject = function fromObject(object) {
            if (object instanceof $root.Capability)
                return object;
            var message = new $root.Capability();
            if (object.writable != null)
                message.writable = Boolean(object.writable);
            if (object.readable != null)
                message.readable = Boolean(object.readable);
            if (object.executable != null)
                message.executable = Boolean(object.executable);
            return message;
        };
    
        /**
         * Creates a plain object from a Capability message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Capability
         * @static
         * @param {Capability} message Capability
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Capability.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.writable = false;
                object.readable = false;
                object.executable = false;
            }
            if (message.writable != null && message.hasOwnProperty("writable"))
                object.writable = message.writable;
            if (message.readable != null && message.hasOwnProperty("readable"))
                object.readable = message.readable;
            if (message.executable != null && message.hasOwnProperty("executable"))
                object.executable = message.executable;
            return object;
        };
    
        /**
         * Converts this Capability to JSON.
         * @function toJSON
         * @memberof Capability
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Capability.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Capability;
    })();
    
    $root.Mode = (function() {
    
        /**
         * Properties of a Mode.
         * @exports IMode
         * @interface IMode
         * @property {ICapability|null} [owner] Mode owner
         * @property {ICapability|null} [group] Mode group
         * @property {ICapability|null} [others] Mode others
         */
    
        /**
         * Constructs a new Mode.
         * @exports Mode
         * @classdesc Represents a Mode.
         * @implements IMode
         * @constructor
         * @param {IMode=} [properties] Properties to set
         */
        function Mode(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Mode owner.
         * @member {ICapability|null|undefined} owner
         * @memberof Mode
         * @instance
         */
        Mode.prototype.owner = null;
    
        /**
         * Mode group.
         * @member {ICapability|null|undefined} group
         * @memberof Mode
         * @instance
         */
        Mode.prototype.group = null;
    
        /**
         * Mode others.
         * @member {ICapability|null|undefined} others
         * @memberof Mode
         * @instance
         */
        Mode.prototype.others = null;
    
        /**
         * Creates a new Mode instance using the specified properties.
         * @function create
         * @memberof Mode
         * @static
         * @param {IMode=} [properties] Properties to set
         * @returns {Mode} Mode instance
         */
        Mode.create = function create(properties) {
            return new Mode(properties);
        };
    
        /**
         * Encodes the specified Mode message. Does not implicitly {@link Mode.verify|verify} messages.
         * @function encode
         * @memberof Mode
         * @static
         * @param {IMode} message Mode message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Mode.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.owner != null && message.hasOwnProperty("owner"))
                $root.Capability.encode(message.owner, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            if (message.group != null && message.hasOwnProperty("group"))
                $root.Capability.encode(message.group, writer.uint32(/* id 2, wireType 2 =*/18).fork()).ldelim();
            if (message.others != null && message.hasOwnProperty("others"))
                $root.Capability.encode(message.others, writer.uint32(/* id 3, wireType 2 =*/26).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified Mode message, length delimited. Does not implicitly {@link Mode.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Mode
         * @static
         * @param {IMode} message Mode message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Mode.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Mode message from the specified reader or buffer.
         * @function decode
         * @memberof Mode
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Mode} Mode
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Mode.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Mode();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.owner = $root.Capability.decode(reader, reader.uint32());
                    break;
                case 2:
                    message.group = $root.Capability.decode(reader, reader.uint32());
                    break;
                case 3:
                    message.others = $root.Capability.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Mode message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Mode
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Mode} Mode
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Mode.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Mode message.
         * @function verify
         * @memberof Mode
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Mode.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.owner != null && message.hasOwnProperty("owner")) {
                var error = $root.Capability.verify(message.owner);
                if (error)
                    return "owner." + error;
            }
            if (message.group != null && message.hasOwnProperty("group")) {
                var error = $root.Capability.verify(message.group);
                if (error)
                    return "group." + error;
            }
            if (message.others != null && message.hasOwnProperty("others")) {
                var error = $root.Capability.verify(message.others);
                if (error)
                    return "others." + error;
            }
            return null;
        };
    
        /**
         * Creates a Mode message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Mode
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Mode} Mode
         */
        Mode.fromObject = function fromObject(object) {
            if (object instanceof $root.Mode)
                return object;
            var message = new $root.Mode();
            if (object.owner != null) {
                if (typeof object.owner !== "object")
                    throw TypeError(".Mode.owner: object expected");
                message.owner = $root.Capability.fromObject(object.owner);
            }
            if (object.group != null) {
                if (typeof object.group !== "object")
                    throw TypeError(".Mode.group: object expected");
                message.group = $root.Capability.fromObject(object.group);
            }
            if (object.others != null) {
                if (typeof object.others !== "object")
                    throw TypeError(".Mode.others: object expected");
                message.others = $root.Capability.fromObject(object.others);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a Mode message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Mode
         * @static
         * @param {Mode} message Mode
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Mode.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.owner = null;
                object.group = null;
                object.others = null;
            }
            if (message.owner != null && message.hasOwnProperty("owner"))
                object.owner = $root.Capability.toObject(message.owner, options);
            if (message.group != null && message.hasOwnProperty("group"))
                object.group = $root.Capability.toObject(message.group, options);
            if (message.others != null && message.hasOwnProperty("others"))
                object.others = $root.Capability.toObject(message.others, options);
            return object;
        };
    
        /**
         * Converts this Mode to JSON.
         * @function toJSON
         * @memberof Mode
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Mode.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Mode;
    })();
    
    $root.FileStat = (function() {
    
        /**
         * Properties of a FileStat.
         * @exports IFileStat
         * @interface IFileStat
         * @property {IMode|null} [mode] FileStat mode
         * @property {number|null} [uid] FileStat uid
         * @property {number|null} [gid] FileStat gid
         * @property {string|null} [atime] FileStat atime
         * @property {string|null} [ctime] FileStat ctime
         * @property {string|null} [mtime] FileStat mtime
         * @property {string|null} [size] FileStat size
         * @property {boolean|null} [isDirectory] FileStat isDirectory
         * @property {boolean|null} [isFile] FileStat isFile
         * @property {boolean|null} [isSymlink] FileStat isSymlink
         */
    
        /**
         * Constructs a new FileStat.
         * @exports FileStat
         * @classdesc Represents a FileStat.
         * @implements IFileStat
         * @constructor
         * @param {IFileStat=} [properties] Properties to set
         */
        function FileStat(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FileStat mode.
         * @member {IMode|null|undefined} mode
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.mode = null;
    
        /**
         * FileStat uid.
         * @member {number} uid
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.uid = 0;
    
        /**
         * FileStat gid.
         * @member {number} gid
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.gid = 0;
    
        /**
         * FileStat atime.
         * @member {string} atime
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.atime = "";
    
        /**
         * FileStat ctime.
         * @member {string} ctime
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.ctime = "";
    
        /**
         * FileStat mtime.
         * @member {string} mtime
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.mtime = "";
    
        /**
         * FileStat size.
         * @member {string} size
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.size = "";
    
        /**
         * FileStat isDirectory.
         * @member {boolean} isDirectory
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.isDirectory = false;
    
        /**
         * FileStat isFile.
         * @member {boolean} isFile
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.isFile = false;
    
        /**
         * FileStat isSymlink.
         * @member {boolean} isSymlink
         * @memberof FileStat
         * @instance
         */
        FileStat.prototype.isSymlink = false;
    
        /**
         * Creates a new FileStat instance using the specified properties.
         * @function create
         * @memberof FileStat
         * @static
         * @param {IFileStat=} [properties] Properties to set
         * @returns {FileStat} FileStat instance
         */
        FileStat.create = function create(properties) {
            return new FileStat(properties);
        };
    
        /**
         * Encodes the specified FileStat message. Does not implicitly {@link FileStat.verify|verify} messages.
         * @function encode
         * @memberof FileStat
         * @static
         * @param {IFileStat} message FileStat message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FileStat.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.mode != null && message.hasOwnProperty("mode"))
                $root.Mode.encode(message.mode, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            if (message.uid != null && message.hasOwnProperty("uid"))
                writer.uint32(/* id 2, wireType 0 =*/16).int32(message.uid);
            if (message.gid != null && message.hasOwnProperty("gid"))
                writer.uint32(/* id 3, wireType 0 =*/24).int32(message.gid);
            if (message.atime != null && message.hasOwnProperty("atime"))
                writer.uint32(/* id 4, wireType 2 =*/34).string(message.atime);
            if (message.ctime != null && message.hasOwnProperty("ctime"))
                writer.uint32(/* id 5, wireType 2 =*/42).string(message.ctime);
            if (message.mtime != null && message.hasOwnProperty("mtime"))
                writer.uint32(/* id 6, wireType 2 =*/50).string(message.mtime);
            if (message.size != null && message.hasOwnProperty("size"))
                writer.uint32(/* id 7, wireType 2 =*/58).string(message.size);
            if (message.isDirectory != null && message.hasOwnProperty("isDirectory"))
                writer.uint32(/* id 8, wireType 0 =*/64).bool(message.isDirectory);
            if (message.isFile != null && message.hasOwnProperty("isFile"))
                writer.uint32(/* id 9, wireType 0 =*/72).bool(message.isFile);
            if (message.isSymlink != null && message.hasOwnProperty("isSymlink"))
                writer.uint32(/* id 10, wireType 0 =*/80).bool(message.isSymlink);
            return writer;
        };
    
        /**
         * Encodes the specified FileStat message, length delimited. Does not implicitly {@link FileStat.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FileStat
         * @static
         * @param {IFileStat} message FileStat message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FileStat.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FileStat message from the specified reader or buffer.
         * @function decode
         * @memberof FileStat
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FileStat} FileStat
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FileStat.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FileStat();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.mode = $root.Mode.decode(reader, reader.uint32());
                    break;
                case 2:
                    message.uid = reader.int32();
                    break;
                case 3:
                    message.gid = reader.int32();
                    break;
                case 4:
                    message.atime = reader.string();
                    break;
                case 5:
                    message.ctime = reader.string();
                    break;
                case 6:
                    message.mtime = reader.string();
                    break;
                case 7:
                    message.size = reader.string();
                    break;
                case 8:
                    message.isDirectory = reader.bool();
                    break;
                case 9:
                    message.isFile = reader.bool();
                    break;
                case 10:
                    message.isSymlink = reader.bool();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FileStat message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FileStat
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FileStat} FileStat
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FileStat.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FileStat message.
         * @function verify
         * @memberof FileStat
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FileStat.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.mode != null && message.hasOwnProperty("mode")) {
                var error = $root.Mode.verify(message.mode);
                if (error)
                    return "mode." + error;
            }
            if (message.uid != null && message.hasOwnProperty("uid"))
                if (!$util.isInteger(message.uid))
                    return "uid: integer expected";
            if (message.gid != null && message.hasOwnProperty("gid"))
                if (!$util.isInteger(message.gid))
                    return "gid: integer expected";
            if (message.atime != null && message.hasOwnProperty("atime"))
                if (!$util.isString(message.atime))
                    return "atime: string expected";
            if (message.ctime != null && message.hasOwnProperty("ctime"))
                if (!$util.isString(message.ctime))
                    return "ctime: string expected";
            if (message.mtime != null && message.hasOwnProperty("mtime"))
                if (!$util.isString(message.mtime))
                    return "mtime: string expected";
            if (message.size != null && message.hasOwnProperty("size"))
                if (!$util.isString(message.size))
                    return "size: string expected";
            if (message.isDirectory != null && message.hasOwnProperty("isDirectory"))
                if (typeof message.isDirectory !== "boolean")
                    return "isDirectory: boolean expected";
            if (message.isFile != null && message.hasOwnProperty("isFile"))
                if (typeof message.isFile !== "boolean")
                    return "isFile: boolean expected";
            if (message.isSymlink != null && message.hasOwnProperty("isSymlink"))
                if (typeof message.isSymlink !== "boolean")
                    return "isSymlink: boolean expected";
            return null;
        };
    
        /**
         * Creates a FileStat message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FileStat
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FileStat} FileStat
         */
        FileStat.fromObject = function fromObject(object) {
            if (object instanceof $root.FileStat)
                return object;
            var message = new $root.FileStat();
            if (object.mode != null) {
                if (typeof object.mode !== "object")
                    throw TypeError(".FileStat.mode: object expected");
                message.mode = $root.Mode.fromObject(object.mode);
            }
            if (object.uid != null)
                message.uid = object.uid | 0;
            if (object.gid != null)
                message.gid = object.gid | 0;
            if (object.atime != null)
                message.atime = String(object.atime);
            if (object.ctime != null)
                message.ctime = String(object.ctime);
            if (object.mtime != null)
                message.mtime = String(object.mtime);
            if (object.size != null)
                message.size = String(object.size);
            if (object.isDirectory != null)
                message.isDirectory = Boolean(object.isDirectory);
            if (object.isFile != null)
                message.isFile = Boolean(object.isFile);
            if (object.isSymlink != null)
                message.isSymlink = Boolean(object.isSymlink);
            return message;
        };
    
        /**
         * Creates a plain object from a FileStat message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FileStat
         * @static
         * @param {FileStat} message FileStat
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FileStat.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.mode = null;
                object.uid = 0;
                object.gid = 0;
                object.atime = "";
                object.ctime = "";
                object.mtime = "";
                object.size = "";
                object.isDirectory = false;
                object.isFile = false;
                object.isSymlink = false;
            }
            if (message.mode != null && message.hasOwnProperty("mode"))
                object.mode = $root.Mode.toObject(message.mode, options);
            if (message.uid != null && message.hasOwnProperty("uid"))
                object.uid = message.uid;
            if (message.gid != null && message.hasOwnProperty("gid"))
                object.gid = message.gid;
            if (message.atime != null && message.hasOwnProperty("atime"))
                object.atime = message.atime;
            if (message.ctime != null && message.hasOwnProperty("ctime"))
                object.ctime = message.ctime;
            if (message.mtime != null && message.hasOwnProperty("mtime"))
                object.mtime = message.mtime;
            if (message.size != null && message.hasOwnProperty("size"))
                object.size = message.size;
            if (message.isDirectory != null && message.hasOwnProperty("isDirectory"))
                object.isDirectory = message.isDirectory;
            if (message.isFile != null && message.hasOwnProperty("isFile"))
                object.isFile = message.isFile;
            if (message.isSymlink != null && message.hasOwnProperty("isSymlink"))
                object.isSymlink = message.isSymlink;
            return object;
        };
    
        /**
         * Converts this FileStat to JSON.
         * @function toJSON
         * @memberof FileStat
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FileStat.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FileStat;
    })();
    
    $root.FileItem = (function() {
    
        /**
         * Properties of a FileItem.
         * @exports IFileItem
         * @interface IFileItem
         * @property {string|null} [id] FileItem id
         * @property {string|null} [parent] FileItem parent
         * @property {string|null} [name] FileItem name
         * @property {string|null} [fullPath] FileItem fullPath
         * @property {IFileStat|null} [stat] FileItem stat
         * @property {boolean|null} [hasLinkPath] FileItem hasLinkPath
         * @property {string|null} [linkPath] FileItem linkPath
         */
    
        /**
         * Constructs a new FileItem.
         * @exports FileItem
         * @classdesc Represents a FileItem.
         * @implements IFileItem
         * @constructor
         * @param {IFileItem=} [properties] Properties to set
         */
        function FileItem(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FileItem id.
         * @member {string} id
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.id = "";
    
        /**
         * FileItem parent.
         * @member {string} parent
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.parent = "";
    
        /**
         * FileItem name.
         * @member {string} name
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.name = "";
    
        /**
         * FileItem fullPath.
         * @member {string} fullPath
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.fullPath = "";
    
        /**
         * FileItem stat.
         * @member {IFileStat|null|undefined} stat
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.stat = null;
    
        /**
         * FileItem hasLinkPath.
         * @member {boolean} hasLinkPath
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.hasLinkPath = false;
    
        /**
         * FileItem linkPath.
         * @member {string} linkPath
         * @memberof FileItem
         * @instance
         */
        FileItem.prototype.linkPath = "";
    
        /**
         * Creates a new FileItem instance using the specified properties.
         * @function create
         * @memberof FileItem
         * @static
         * @param {IFileItem=} [properties] Properties to set
         * @returns {FileItem} FileItem instance
         */
        FileItem.create = function create(properties) {
            return new FileItem(properties);
        };
    
        /**
         * Encodes the specified FileItem message. Does not implicitly {@link FileItem.verify|verify} messages.
         * @function encode
         * @memberof FileItem
         * @static
         * @param {IFileItem} message FileItem message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FileItem.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.id != null && message.hasOwnProperty("id"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.id);
            if (message.parent != null && message.hasOwnProperty("parent"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.parent);
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 3, wireType 2 =*/26).string(message.name);
            if (message.fullPath != null && message.hasOwnProperty("fullPath"))
                writer.uint32(/* id 4, wireType 2 =*/34).string(message.fullPath);
            if (message.stat != null && message.hasOwnProperty("stat"))
                $root.FileStat.encode(message.stat, writer.uint32(/* id 5, wireType 2 =*/42).fork()).ldelim();
            if (message.hasLinkPath != null && message.hasOwnProperty("hasLinkPath"))
                writer.uint32(/* id 6, wireType 0 =*/48).bool(message.hasLinkPath);
            if (message.linkPath != null && message.hasOwnProperty("linkPath"))
                writer.uint32(/* id 7, wireType 2 =*/58).string(message.linkPath);
            return writer;
        };
    
        /**
         * Encodes the specified FileItem message, length delimited. Does not implicitly {@link FileItem.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FileItem
         * @static
         * @param {IFileItem} message FileItem message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FileItem.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FileItem message from the specified reader or buffer.
         * @function decode
         * @memberof FileItem
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FileItem} FileItem
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FileItem.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FileItem();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.id = reader.string();
                    break;
                case 2:
                    message.parent = reader.string();
                    break;
                case 3:
                    message.name = reader.string();
                    break;
                case 4:
                    message.fullPath = reader.string();
                    break;
                case 5:
                    message.stat = $root.FileStat.decode(reader, reader.uint32());
                    break;
                case 6:
                    message.hasLinkPath = reader.bool();
                    break;
                case 7:
                    message.linkPath = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FileItem message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FileItem
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FileItem} FileItem
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FileItem.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FileItem message.
         * @function verify
         * @memberof FileItem
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FileItem.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.id != null && message.hasOwnProperty("id"))
                if (!$util.isString(message.id))
                    return "id: string expected";
            if (message.parent != null && message.hasOwnProperty("parent"))
                if (!$util.isString(message.parent))
                    return "parent: string expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            if (message.fullPath != null && message.hasOwnProperty("fullPath"))
                if (!$util.isString(message.fullPath))
                    return "fullPath: string expected";
            if (message.stat != null && message.hasOwnProperty("stat")) {
                var error = $root.FileStat.verify(message.stat);
                if (error)
                    return "stat." + error;
            }
            if (message.hasLinkPath != null && message.hasOwnProperty("hasLinkPath"))
                if (typeof message.hasLinkPath !== "boolean")
                    return "hasLinkPath: boolean expected";
            if (message.linkPath != null && message.hasOwnProperty("linkPath"))
                if (!$util.isString(message.linkPath))
                    return "linkPath: string expected";
            return null;
        };
    
        /**
         * Creates a FileItem message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FileItem
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FileItem} FileItem
         */
        FileItem.fromObject = function fromObject(object) {
            if (object instanceof $root.FileItem)
                return object;
            var message = new $root.FileItem();
            if (object.id != null)
                message.id = String(object.id);
            if (object.parent != null)
                message.parent = String(object.parent);
            if (object.name != null)
                message.name = String(object.name);
            if (object.fullPath != null)
                message.fullPath = String(object.fullPath);
            if (object.stat != null) {
                if (typeof object.stat !== "object")
                    throw TypeError(".FileItem.stat: object expected");
                message.stat = $root.FileStat.fromObject(object.stat);
            }
            if (object.hasLinkPath != null)
                message.hasLinkPath = Boolean(object.hasLinkPath);
            if (object.linkPath != null)
                message.linkPath = String(object.linkPath);
            return message;
        };
    
        /**
         * Creates a plain object from a FileItem message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FileItem
         * @static
         * @param {FileItem} message FileItem
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FileItem.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.id = "";
                object.parent = "";
                object.name = "";
                object.fullPath = "";
                object.stat = null;
                object.hasLinkPath = false;
                object.linkPath = "";
            }
            if (message.id != null && message.hasOwnProperty("id"))
                object.id = message.id;
            if (message.parent != null && message.hasOwnProperty("parent"))
                object.parent = message.parent;
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            if (message.fullPath != null && message.hasOwnProperty("fullPath"))
                object.fullPath = message.fullPath;
            if (message.stat != null && message.hasOwnProperty("stat"))
                object.stat = $root.FileStat.toObject(message.stat, options);
            if (message.hasLinkPath != null && message.hasOwnProperty("hasLinkPath"))
                object.hasLinkPath = message.hasLinkPath;
            if (message.linkPath != null && message.hasOwnProperty("linkPath"))
                object.linkPath = message.linkPath;
            return object;
        };
    
        /**
         * Converts this FileItem to JSON.
         * @function toJSON
         * @memberof FileItem
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FileItem.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FileItem;
    })();
    
    $root.FileList = (function() {
    
        /**
         * Properties of a FileList.
         * @exports IFileList
         * @interface IFileList
         * @property {string|null} [location] FileList location
         * @property {Array.<IFileItem>|null} [items] FileList items
         */
    
        /**
         * Constructs a new FileList.
         * @exports FileList
         * @classdesc Represents a FileList.
         * @implements IFileList
         * @constructor
         * @param {IFileList=} [properties] Properties to set
         */
        function FileList(properties) {
            this.items = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FileList location.
         * @member {string} location
         * @memberof FileList
         * @instance
         */
        FileList.prototype.location = "";
    
        /**
         * FileList items.
         * @member {Array.<IFileItem>} items
         * @memberof FileList
         * @instance
         */
        FileList.prototype.items = $util.emptyArray;
    
        /**
         * Creates a new FileList instance using the specified properties.
         * @function create
         * @memberof FileList
         * @static
         * @param {IFileList=} [properties] Properties to set
         * @returns {FileList} FileList instance
         */
        FileList.create = function create(properties) {
            return new FileList(properties);
        };
    
        /**
         * Encodes the specified FileList message. Does not implicitly {@link FileList.verify|verify} messages.
         * @function encode
         * @memberof FileList
         * @static
         * @param {IFileList} message FileList message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FileList.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.location != null && message.hasOwnProperty("location"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.location);
            if (message.items != null && message.items.length)
                for (var i = 0; i < message.items.length; ++i)
                    $root.FileItem.encode(message.items[i], writer.uint32(/* id 2, wireType 2 =*/18).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FileList message, length delimited. Does not implicitly {@link FileList.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FileList
         * @static
         * @param {IFileList} message FileList message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FileList.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FileList message from the specified reader or buffer.
         * @function decode
         * @memberof FileList
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FileList} FileList
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FileList.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FileList();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.location = reader.string();
                    break;
                case 2:
                    if (!(message.items && message.items.length))
                        message.items = [];
                    message.items.push($root.FileItem.decode(reader, reader.uint32()));
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FileList message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FileList
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FileList} FileList
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FileList.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FileList message.
         * @function verify
         * @memberof FileList
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FileList.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.location != null && message.hasOwnProperty("location"))
                if (!$util.isString(message.location))
                    return "location: string expected";
            if (message.items != null && message.hasOwnProperty("items")) {
                if (!Array.isArray(message.items))
                    return "items: array expected";
                for (var i = 0; i < message.items.length; ++i) {
                    var error = $root.FileItem.verify(message.items[i]);
                    if (error)
                        return "items." + error;
                }
            }
            return null;
        };
    
        /**
         * Creates a FileList message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FileList
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FileList} FileList
         */
        FileList.fromObject = function fromObject(object) {
            if (object instanceof $root.FileList)
                return object;
            var message = new $root.FileList();
            if (object.location != null)
                message.location = String(object.location);
            if (object.items) {
                if (!Array.isArray(object.items))
                    throw TypeError(".FileList.items: array expected");
                message.items = [];
                for (var i = 0; i < object.items.length; ++i) {
                    if (typeof object.items[i] !== "object")
                        throw TypeError(".FileList.items: object expected");
                    message.items[i] = $root.FileItem.fromObject(object.items[i]);
                }
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FileList message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FileList
         * @static
         * @param {FileList} message FileList
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FileList.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.items = [];
            if (options.defaults)
                object.location = "";
            if (message.location != null && message.hasOwnProperty("location"))
                object.location = message.location;
            if (message.items && message.items.length) {
                object.items = [];
                for (var j = 0; j < message.items.length; ++j)
                    object.items[j] = $root.FileItem.toObject(message.items[j], options);
            }
            return object;
        };
    
        /**
         * Converts this FileList to JSON.
         * @function toJSON
         * @memberof FileList
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FileList.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FileList;
    })();
    
    $root.LocationHistory = (function() {
    
        /**
         * Properties of a LocationHistory.
         * @exports ILocationHistory
         * @interface ILocationHistory
         * @property {Array.<ILocationRecord>|null} [records] LocationHistory records
         * @property {number|null} [maxRecordNumber] LocationHistory maxRecordNumber
         */
    
        /**
         * Constructs a new LocationHistory.
         * @exports LocationHistory
         * @classdesc Represents a LocationHistory.
         * @implements ILocationHistory
         * @constructor
         * @param {ILocationHistory=} [properties] Properties to set
         */
        function LocationHistory(properties) {
            this.records = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * LocationHistory records.
         * @member {Array.<ILocationRecord>} records
         * @memberof LocationHistory
         * @instance
         */
        LocationHistory.prototype.records = $util.emptyArray;
    
        /**
         * LocationHistory maxRecordNumber.
         * @member {number} maxRecordNumber
         * @memberof LocationHistory
         * @instance
         */
        LocationHistory.prototype.maxRecordNumber = 0;
    
        /**
         * Creates a new LocationHistory instance using the specified properties.
         * @function create
         * @memberof LocationHistory
         * @static
         * @param {ILocationHistory=} [properties] Properties to set
         * @returns {LocationHistory} LocationHistory instance
         */
        LocationHistory.create = function create(properties) {
            return new LocationHistory(properties);
        };
    
        /**
         * Encodes the specified LocationHistory message. Does not implicitly {@link LocationHistory.verify|verify} messages.
         * @function encode
         * @memberof LocationHistory
         * @static
         * @param {ILocationHistory} message LocationHistory message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        LocationHistory.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.records != null && message.records.length)
                for (var i = 0; i < message.records.length; ++i)
                    $root.LocationRecord.encode(message.records[i], writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            if (message.maxRecordNumber != null && message.hasOwnProperty("maxRecordNumber"))
                writer.uint32(/* id 2, wireType 0 =*/16).int32(message.maxRecordNumber);
            return writer;
        };
    
        /**
         * Encodes the specified LocationHistory message, length delimited. Does not implicitly {@link LocationHistory.verify|verify} messages.
         * @function encodeDelimited
         * @memberof LocationHistory
         * @static
         * @param {ILocationHistory} message LocationHistory message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        LocationHistory.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a LocationHistory message from the specified reader or buffer.
         * @function decode
         * @memberof LocationHistory
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {LocationHistory} LocationHistory
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        LocationHistory.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.LocationHistory();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    if (!(message.records && message.records.length))
                        message.records = [];
                    message.records.push($root.LocationRecord.decode(reader, reader.uint32()));
                    break;
                case 2:
                    message.maxRecordNumber = reader.int32();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a LocationHistory message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof LocationHistory
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {LocationHistory} LocationHistory
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        LocationHistory.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a LocationHistory message.
         * @function verify
         * @memberof LocationHistory
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        LocationHistory.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.records != null && message.hasOwnProperty("records")) {
                if (!Array.isArray(message.records))
                    return "records: array expected";
                for (var i = 0; i < message.records.length; ++i) {
                    var error = $root.LocationRecord.verify(message.records[i]);
                    if (error)
                        return "records." + error;
                }
            }
            if (message.maxRecordNumber != null && message.hasOwnProperty("maxRecordNumber"))
                if (!$util.isInteger(message.maxRecordNumber))
                    return "maxRecordNumber: integer expected";
            return null;
        };
    
        /**
         * Creates a LocationHistory message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof LocationHistory
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {LocationHistory} LocationHistory
         */
        LocationHistory.fromObject = function fromObject(object) {
            if (object instanceof $root.LocationHistory)
                return object;
            var message = new $root.LocationHistory();
            if (object.records) {
                if (!Array.isArray(object.records))
                    throw TypeError(".LocationHistory.records: array expected");
                message.records = [];
                for (var i = 0; i < object.records.length; ++i) {
                    if (typeof object.records[i] !== "object")
                        throw TypeError(".LocationHistory.records: object expected");
                    message.records[i] = $root.LocationRecord.fromObject(object.records[i]);
                }
            }
            if (object.maxRecordNumber != null)
                message.maxRecordNumber = object.maxRecordNumber | 0;
            return message;
        };
    
        /**
         * Creates a plain object from a LocationHistory message. Also converts values to other types if specified.
         * @function toObject
         * @memberof LocationHistory
         * @static
         * @param {LocationHistory} message LocationHistory
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        LocationHistory.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.records = [];
            if (options.defaults)
                object.maxRecordNumber = 0;
            if (message.records && message.records.length) {
                object.records = [];
                for (var j = 0; j < message.records.length; ++j)
                    object.records[j] = $root.LocationRecord.toObject(message.records[j], options);
            }
            if (message.maxRecordNumber != null && message.hasOwnProperty("maxRecordNumber"))
                object.maxRecordNumber = message.maxRecordNumber;
            return object;
        };
    
        /**
         * Converts this LocationHistory to JSON.
         * @function toJSON
         * @memberof LocationHistory
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        LocationHistory.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return LocationHistory;
    })();
    
    $root.LocationRecord = (function() {
    
        /**
         * Properties of a LocationRecord.
         * @exports ILocationRecord
         * @interface ILocationRecord
         * @property {string|null} [location] LocationRecord location
         * @property {string|null} [timestamp] LocationRecord timestamp
         */
    
        /**
         * Constructs a new LocationRecord.
         * @exports LocationRecord
         * @classdesc Represents a LocationRecord.
         * @implements ILocationRecord
         * @constructor
         * @param {ILocationRecord=} [properties] Properties to set
         */
        function LocationRecord(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * LocationRecord location.
         * @member {string} location
         * @memberof LocationRecord
         * @instance
         */
        LocationRecord.prototype.location = "";
    
        /**
         * LocationRecord timestamp.
         * @member {string} timestamp
         * @memberof LocationRecord
         * @instance
         */
        LocationRecord.prototype.timestamp = "";
    
        /**
         * Creates a new LocationRecord instance using the specified properties.
         * @function create
         * @memberof LocationRecord
         * @static
         * @param {ILocationRecord=} [properties] Properties to set
         * @returns {LocationRecord} LocationRecord instance
         */
        LocationRecord.create = function create(properties) {
            return new LocationRecord(properties);
        };
    
        /**
         * Encodes the specified LocationRecord message. Does not implicitly {@link LocationRecord.verify|verify} messages.
         * @function encode
         * @memberof LocationRecord
         * @static
         * @param {ILocationRecord} message LocationRecord message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        LocationRecord.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.location != null && message.hasOwnProperty("location"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.location);
            if (message.timestamp != null && message.hasOwnProperty("timestamp"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.timestamp);
            return writer;
        };
    
        /**
         * Encodes the specified LocationRecord message, length delimited. Does not implicitly {@link LocationRecord.verify|verify} messages.
         * @function encodeDelimited
         * @memberof LocationRecord
         * @static
         * @param {ILocationRecord} message LocationRecord message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        LocationRecord.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a LocationRecord message from the specified reader or buffer.
         * @function decode
         * @memberof LocationRecord
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {LocationRecord} LocationRecord
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        LocationRecord.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.LocationRecord();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.location = reader.string();
                    break;
                case 2:
                    message.timestamp = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a LocationRecord message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof LocationRecord
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {LocationRecord} LocationRecord
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        LocationRecord.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a LocationRecord message.
         * @function verify
         * @memberof LocationRecord
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        LocationRecord.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.location != null && message.hasOwnProperty("location"))
                if (!$util.isString(message.location))
                    return "location: string expected";
            if (message.timestamp != null && message.hasOwnProperty("timestamp"))
                if (!$util.isString(message.timestamp))
                    return "timestamp: string expected";
            return null;
        };
    
        /**
         * Creates a LocationRecord message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof LocationRecord
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {LocationRecord} LocationRecord
         */
        LocationRecord.fromObject = function fromObject(object) {
            if (object instanceof $root.LocationRecord)
                return object;
            var message = new $root.LocationRecord();
            if (object.location != null)
                message.location = String(object.location);
            if (object.timestamp != null)
                message.timestamp = String(object.timestamp);
            return message;
        };
    
        /**
         * Creates a plain object from a LocationRecord message. Also converts values to other types if specified.
         * @function toObject
         * @memberof LocationRecord
         * @static
         * @param {LocationRecord} message LocationRecord
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        LocationRecord.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.location = "";
                object.timestamp = "";
            }
            if (message.location != null && message.hasOwnProperty("location"))
                object.location = message.location;
            if (message.timestamp != null && message.hasOwnProperty("timestamp"))
                object.timestamp = message.timestamp;
            return object;
        };
    
        /**
         * Converts this LocationRecord to JSON.
         * @function toJSON
         * @memberof LocationRecord
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        LocationRecord.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return LocationRecord;
    })();
    
    $root.Filer = (function() {
    
        /**
         * Properties of a Filer.
         * @exports IFiler
         * @interface IFiler
         * @property {string|null} [id] Filer id
         * @property {string|null} [name] Filer name
         * @property {IFileList|null} [fileList] Filer fileList
         * @property {ILocationHistory|null} [history] Filer history
         * @property {Array.<string>|null} [markedItems] Filer markedItems
         * @property {SortType|null} [sortOrder] Filer sortOrder
         */
    
        /**
         * Constructs a new Filer.
         * @exports Filer
         * @classdesc Represents a Filer.
         * @implements IFiler
         * @constructor
         * @param {IFiler=} [properties] Properties to set
         */
        function Filer(properties) {
            this.markedItems = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Filer id.
         * @member {string} id
         * @memberof Filer
         * @instance
         */
        Filer.prototype.id = "";
    
        /**
         * Filer name.
         * @member {string} name
         * @memberof Filer
         * @instance
         */
        Filer.prototype.name = "";
    
        /**
         * Filer fileList.
         * @member {IFileList|null|undefined} fileList
         * @memberof Filer
         * @instance
         */
        Filer.prototype.fileList = null;
    
        /**
         * Filer history.
         * @member {ILocationHistory|null|undefined} history
         * @memberof Filer
         * @instance
         */
        Filer.prototype.history = null;
    
        /**
         * Filer markedItems.
         * @member {Array.<string>} markedItems
         * @memberof Filer
         * @instance
         */
        Filer.prototype.markedItems = $util.emptyArray;
    
        /**
         * Filer sortOrder.
         * @member {SortType} sortOrder
         * @memberof Filer
         * @instance
         */
        Filer.prototype.sortOrder = 0;
    
        /**
         * Creates a new Filer instance using the specified properties.
         * @function create
         * @memberof Filer
         * @static
         * @param {IFiler=} [properties] Properties to set
         * @returns {Filer} Filer instance
         */
        Filer.create = function create(properties) {
            return new Filer(properties);
        };
    
        /**
         * Encodes the specified Filer message. Does not implicitly {@link Filer.verify|verify} messages.
         * @function encode
         * @memberof Filer
         * @static
         * @param {IFiler} message Filer message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Filer.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.id != null && message.hasOwnProperty("id"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.id);
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.name);
            if (message.fileList != null && message.hasOwnProperty("fileList"))
                $root.FileList.encode(message.fileList, writer.uint32(/* id 3, wireType 2 =*/26).fork()).ldelim();
            if (message.history != null && message.hasOwnProperty("history"))
                $root.LocationHistory.encode(message.history, writer.uint32(/* id 4, wireType 2 =*/34).fork()).ldelim();
            if (message.markedItems != null && message.markedItems.length)
                for (var i = 0; i < message.markedItems.length; ++i)
                    writer.uint32(/* id 5, wireType 2 =*/42).string(message.markedItems[i]);
            if (message.sortOrder != null && message.hasOwnProperty("sortOrder"))
                writer.uint32(/* id 6, wireType 0 =*/48).int32(message.sortOrder);
            return writer;
        };
    
        /**
         * Encodes the specified Filer message, length delimited. Does not implicitly {@link Filer.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Filer
         * @static
         * @param {IFiler} message Filer message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Filer.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Filer message from the specified reader or buffer.
         * @function decode
         * @memberof Filer
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Filer} Filer
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Filer.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Filer();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.id = reader.string();
                    break;
                case 2:
                    message.name = reader.string();
                    break;
                case 3:
                    message.fileList = $root.FileList.decode(reader, reader.uint32());
                    break;
                case 4:
                    message.history = $root.LocationHistory.decode(reader, reader.uint32());
                    break;
                case 5:
                    if (!(message.markedItems && message.markedItems.length))
                        message.markedItems = [];
                    message.markedItems.push(reader.string());
                    break;
                case 6:
                    message.sortOrder = reader.int32();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Filer message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Filer
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Filer} Filer
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Filer.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Filer message.
         * @function verify
         * @memberof Filer
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Filer.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.id != null && message.hasOwnProperty("id"))
                if (!$util.isString(message.id))
                    return "id: string expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            if (message.fileList != null && message.hasOwnProperty("fileList")) {
                var error = $root.FileList.verify(message.fileList);
                if (error)
                    return "fileList." + error;
            }
            if (message.history != null && message.hasOwnProperty("history")) {
                var error = $root.LocationHistory.verify(message.history);
                if (error)
                    return "history." + error;
            }
            if (message.markedItems != null && message.hasOwnProperty("markedItems")) {
                if (!Array.isArray(message.markedItems))
                    return "markedItems: array expected";
                for (var i = 0; i < message.markedItems.length; ++i)
                    if (!$util.isString(message.markedItems[i]))
                        return "markedItems: string[] expected";
            }
            if (message.sortOrder != null && message.hasOwnProperty("sortOrder"))
                switch (message.sortOrder) {
                default:
                    return "sortOrder: enum value expected";
                case 0:
                case 1:
                case 2:
                    break;
                }
            return null;
        };
    
        /**
         * Creates a Filer message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Filer
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Filer} Filer
         */
        Filer.fromObject = function fromObject(object) {
            if (object instanceof $root.Filer)
                return object;
            var message = new $root.Filer();
            if (object.id != null)
                message.id = String(object.id);
            if (object.name != null)
                message.name = String(object.name);
            if (object.fileList != null) {
                if (typeof object.fileList !== "object")
                    throw TypeError(".Filer.fileList: object expected");
                message.fileList = $root.FileList.fromObject(object.fileList);
            }
            if (object.history != null) {
                if (typeof object.history !== "object")
                    throw TypeError(".Filer.history: object expected");
                message.history = $root.LocationHistory.fromObject(object.history);
            }
            if (object.markedItems) {
                if (!Array.isArray(object.markedItems))
                    throw TypeError(".Filer.markedItems: array expected");
                message.markedItems = [];
                for (var i = 0; i < object.markedItems.length; ++i)
                    message.markedItems[i] = String(object.markedItems[i]);
            }
            switch (object.sortOrder) {
            case "Name":
            case 0:
                message.sortOrder = 0;
                break;
            case "Size":
            case 1:
                message.sortOrder = 1;
                break;
            case "Date":
            case 2:
                message.sortOrder = 2;
                break;
            }
            return message;
        };
    
        /**
         * Creates a plain object from a Filer message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Filer
         * @static
         * @param {Filer} message Filer
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Filer.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.markedItems = [];
            if (options.defaults) {
                object.id = "";
                object.name = "";
                object.fileList = null;
                object.history = null;
                object.sortOrder = options.enums === String ? "Name" : 0;
            }
            if (message.id != null && message.hasOwnProperty("id"))
                object.id = message.id;
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            if (message.fileList != null && message.hasOwnProperty("fileList"))
                object.fileList = $root.FileList.toObject(message.fileList, options);
            if (message.history != null && message.hasOwnProperty("history"))
                object.history = $root.LocationHistory.toObject(message.history, options);
            if (message.markedItems && message.markedItems.length) {
                object.markedItems = [];
                for (var j = 0; j < message.markedItems.length; ++j)
                    object.markedItems[j] = message.markedItems[j];
            }
            if (message.sortOrder != null && message.hasOwnProperty("sortOrder"))
                object.sortOrder = options.enums === String ? $root.SortType[message.sortOrder] : message.sortOrder;
            return object;
        };
    
        /**
         * Converts this Filer to JSON.
         * @function toJSON
         * @memberof Filer
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Filer.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Filer;
    })();
    
    $root.FilerMakeRequest = (function() {
    
        /**
         * Properties of a FilerMakeRequest.
         * @exports IFilerMakeRequest
         * @interface IFilerMakeRequest
         * @property {string|null} [initialLocation] FilerMakeRequest initialLocation
         * @property {string|null} [name] FilerMakeRequest name
         */
    
        /**
         * Constructs a new FilerMakeRequest.
         * @exports FilerMakeRequest
         * @classdesc Represents a FilerMakeRequest.
         * @implements IFilerMakeRequest
         * @constructor
         * @param {IFilerMakeRequest=} [properties] Properties to set
         */
        function FilerMakeRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerMakeRequest initialLocation.
         * @member {string} initialLocation
         * @memberof FilerMakeRequest
         * @instance
         */
        FilerMakeRequest.prototype.initialLocation = "";
    
        /**
         * FilerMakeRequest name.
         * @member {string} name
         * @memberof FilerMakeRequest
         * @instance
         */
        FilerMakeRequest.prototype.name = "";
    
        /**
         * Creates a new FilerMakeRequest instance using the specified properties.
         * @function create
         * @memberof FilerMakeRequest
         * @static
         * @param {IFilerMakeRequest=} [properties] Properties to set
         * @returns {FilerMakeRequest} FilerMakeRequest instance
         */
        FilerMakeRequest.create = function create(properties) {
            return new FilerMakeRequest(properties);
        };
    
        /**
         * Encodes the specified FilerMakeRequest message. Does not implicitly {@link FilerMakeRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerMakeRequest
         * @static
         * @param {IFilerMakeRequest} message FilerMakeRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMakeRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.initialLocation != null && message.hasOwnProperty("initialLocation"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.initialLocation);
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.name);
            return writer;
        };
    
        /**
         * Encodes the specified FilerMakeRequest message, length delimited. Does not implicitly {@link FilerMakeRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerMakeRequest
         * @static
         * @param {IFilerMakeRequest} message FilerMakeRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMakeRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerMakeRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerMakeRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerMakeRequest} FilerMakeRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMakeRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerMakeRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.initialLocation = reader.string();
                    break;
                case 2:
                    message.name = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerMakeRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerMakeRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerMakeRequest} FilerMakeRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMakeRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerMakeRequest message.
         * @function verify
         * @memberof FilerMakeRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerMakeRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.initialLocation != null && message.hasOwnProperty("initialLocation"))
                if (!$util.isString(message.initialLocation))
                    return "initialLocation: string expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            return null;
        };
    
        /**
         * Creates a FilerMakeRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerMakeRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerMakeRequest} FilerMakeRequest
         */
        FilerMakeRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerMakeRequest)
                return object;
            var message = new $root.FilerMakeRequest();
            if (object.initialLocation != null)
                message.initialLocation = String(object.initialLocation);
            if (object.name != null)
                message.name = String(object.name);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerMakeRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerMakeRequest
         * @static
         * @param {FilerMakeRequest} message FilerMakeRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerMakeRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.initialLocation = "";
                object.name = "";
            }
            if (message.initialLocation != null && message.hasOwnProperty("initialLocation"))
                object.initialLocation = message.initialLocation;
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            return object;
        };
    
        /**
         * Converts this FilerMakeRequest to JSON.
         * @function toJSON
         * @memberof FilerMakeRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerMakeRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerMakeRequest;
    })();
    
    $root.FilerMakeResponse = (function() {
    
        /**
         * Properties of a FilerMakeResponse.
         * @exports IFilerMakeResponse
         * @interface IFilerMakeResponse
         * @property {IFiler|null} [filer] FilerMakeResponse filer
         */
    
        /**
         * Constructs a new FilerMakeResponse.
         * @exports FilerMakeResponse
         * @classdesc Represents a FilerMakeResponse.
         * @implements IFilerMakeResponse
         * @constructor
         * @param {IFilerMakeResponse=} [properties] Properties to set
         */
        function FilerMakeResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerMakeResponse filer.
         * @member {IFiler|null|undefined} filer
         * @memberof FilerMakeResponse
         * @instance
         */
        FilerMakeResponse.prototype.filer = null;
    
        /**
         * Creates a new FilerMakeResponse instance using the specified properties.
         * @function create
         * @memberof FilerMakeResponse
         * @static
         * @param {IFilerMakeResponse=} [properties] Properties to set
         * @returns {FilerMakeResponse} FilerMakeResponse instance
         */
        FilerMakeResponse.create = function create(properties) {
            return new FilerMakeResponse(properties);
        };
    
        /**
         * Encodes the specified FilerMakeResponse message. Does not implicitly {@link FilerMakeResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerMakeResponse
         * @static
         * @param {IFilerMakeResponse} message FilerMakeResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMakeResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.filer != null && message.hasOwnProperty("filer"))
                $root.Filer.encode(message.filer, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FilerMakeResponse message, length delimited. Does not implicitly {@link FilerMakeResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerMakeResponse
         * @static
         * @param {IFilerMakeResponse} message FilerMakeResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMakeResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerMakeResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerMakeResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerMakeResponse} FilerMakeResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMakeResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerMakeResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.filer = $root.Filer.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerMakeResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerMakeResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerMakeResponse} FilerMakeResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMakeResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerMakeResponse message.
         * @function verify
         * @memberof FilerMakeResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerMakeResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.filer != null && message.hasOwnProperty("filer")) {
                var error = $root.Filer.verify(message.filer);
                if (error)
                    return "filer." + error;
            }
            return null;
        };
    
        /**
         * Creates a FilerMakeResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerMakeResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerMakeResponse} FilerMakeResponse
         */
        FilerMakeResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerMakeResponse)
                return object;
            var message = new $root.FilerMakeResponse();
            if (object.filer != null) {
                if (typeof object.filer !== "object")
                    throw TypeError(".FilerMakeResponse.filer: object expected");
                message.filer = $root.Filer.fromObject(object.filer);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerMakeResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerMakeResponse
         * @static
         * @param {FilerMakeResponse} message FilerMakeResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerMakeResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.filer = null;
            if (message.filer != null && message.hasOwnProperty("filer"))
                object.filer = $root.Filer.toObject(message.filer, options);
            return object;
        };
    
        /**
         * Converts this FilerMakeResponse to JSON.
         * @function toJSON
         * @memberof FilerMakeResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerMakeResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerMakeResponse;
    })();
    
    $root.FilerGetRequest = (function() {
    
        /**
         * Properties of a FilerGetRequest.
         * @exports IFilerGetRequest
         * @interface IFilerGetRequest
         * @property {string|null} [name] FilerGetRequest name
         */
    
        /**
         * Constructs a new FilerGetRequest.
         * @exports FilerGetRequest
         * @classdesc Represents a FilerGetRequest.
         * @implements IFilerGetRequest
         * @constructor
         * @param {IFilerGetRequest=} [properties] Properties to set
         */
        function FilerGetRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerGetRequest name.
         * @member {string} name
         * @memberof FilerGetRequest
         * @instance
         */
        FilerGetRequest.prototype.name = "";
    
        /**
         * Creates a new FilerGetRequest instance using the specified properties.
         * @function create
         * @memberof FilerGetRequest
         * @static
         * @param {IFilerGetRequest=} [properties] Properties to set
         * @returns {FilerGetRequest} FilerGetRequest instance
         */
        FilerGetRequest.create = function create(properties) {
            return new FilerGetRequest(properties);
        };
    
        /**
         * Encodes the specified FilerGetRequest message. Does not implicitly {@link FilerGetRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerGetRequest
         * @static
         * @param {IFilerGetRequest} message FilerGetRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerGetRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.name);
            return writer;
        };
    
        /**
         * Encodes the specified FilerGetRequest message, length delimited. Does not implicitly {@link FilerGetRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerGetRequest
         * @static
         * @param {IFilerGetRequest} message FilerGetRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerGetRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerGetRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerGetRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerGetRequest} FilerGetRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerGetRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerGetRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.name = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerGetRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerGetRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerGetRequest} FilerGetRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerGetRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerGetRequest message.
         * @function verify
         * @memberof FilerGetRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerGetRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            return null;
        };
    
        /**
         * Creates a FilerGetRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerGetRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerGetRequest} FilerGetRequest
         */
        FilerGetRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerGetRequest)
                return object;
            var message = new $root.FilerGetRequest();
            if (object.name != null)
                message.name = String(object.name);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerGetRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerGetRequest
         * @static
         * @param {FilerGetRequest} message FilerGetRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerGetRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.name = "";
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            return object;
        };
    
        /**
         * Converts this FilerGetRequest to JSON.
         * @function toJSON
         * @memberof FilerGetRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerGetRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerGetRequest;
    })();
    
    $root.FilerGetResponse = (function() {
    
        /**
         * Properties of a FilerGetResponse.
         * @exports IFilerGetResponse
         * @interface IFilerGetResponse
         * @property {IFiler|null} [filer] FilerGetResponse filer
         */
    
        /**
         * Constructs a new FilerGetResponse.
         * @exports FilerGetResponse
         * @classdesc Represents a FilerGetResponse.
         * @implements IFilerGetResponse
         * @constructor
         * @param {IFilerGetResponse=} [properties] Properties to set
         */
        function FilerGetResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerGetResponse filer.
         * @member {IFiler|null|undefined} filer
         * @memberof FilerGetResponse
         * @instance
         */
        FilerGetResponse.prototype.filer = null;
    
        /**
         * Creates a new FilerGetResponse instance using the specified properties.
         * @function create
         * @memberof FilerGetResponse
         * @static
         * @param {IFilerGetResponse=} [properties] Properties to set
         * @returns {FilerGetResponse} FilerGetResponse instance
         */
        FilerGetResponse.create = function create(properties) {
            return new FilerGetResponse(properties);
        };
    
        /**
         * Encodes the specified FilerGetResponse message. Does not implicitly {@link FilerGetResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerGetResponse
         * @static
         * @param {IFilerGetResponse} message FilerGetResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerGetResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.filer != null && message.hasOwnProperty("filer"))
                $root.Filer.encode(message.filer, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FilerGetResponse message, length delimited. Does not implicitly {@link FilerGetResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerGetResponse
         * @static
         * @param {IFilerGetResponse} message FilerGetResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerGetResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerGetResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerGetResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerGetResponse} FilerGetResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerGetResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerGetResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.filer = $root.Filer.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerGetResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerGetResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerGetResponse} FilerGetResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerGetResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerGetResponse message.
         * @function verify
         * @memberof FilerGetResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerGetResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.filer != null && message.hasOwnProperty("filer")) {
                var error = $root.Filer.verify(message.filer);
                if (error)
                    return "filer." + error;
            }
            return null;
        };
    
        /**
         * Creates a FilerGetResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerGetResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerGetResponse} FilerGetResponse
         */
        FilerGetResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerGetResponse)
                return object;
            var message = new $root.FilerGetResponse();
            if (object.filer != null) {
                if (typeof object.filer !== "object")
                    throw TypeError(".FilerGetResponse.filer: object expected");
                message.filer = $root.Filer.fromObject(object.filer);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerGetResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerGetResponse
         * @static
         * @param {FilerGetResponse} message FilerGetResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerGetResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.filer = null;
            if (message.filer != null && message.hasOwnProperty("filer"))
                object.filer = $root.Filer.toObject(message.filer, options);
            return object;
        };
    
        /**
         * Converts this FilerGetResponse to JSON.
         * @function toJSON
         * @memberof FilerGetResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerGetResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerGetResponse;
    })();
    
    $root.FilerMoveParentRequest = (function() {
    
        /**
         * Properties of a FilerMoveParentRequest.
         * @exports IFilerMoveParentRequest
         * @interface IFilerMoveParentRequest
         * @property {string|null} [name] FilerMoveParentRequest name
         */
    
        /**
         * Constructs a new FilerMoveParentRequest.
         * @exports FilerMoveParentRequest
         * @classdesc Represents a FilerMoveParentRequest.
         * @implements IFilerMoveParentRequest
         * @constructor
         * @param {IFilerMoveParentRequest=} [properties] Properties to set
         */
        function FilerMoveParentRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerMoveParentRequest name.
         * @member {string} name
         * @memberof FilerMoveParentRequest
         * @instance
         */
        FilerMoveParentRequest.prototype.name = "";
    
        /**
         * Creates a new FilerMoveParentRequest instance using the specified properties.
         * @function create
         * @memberof FilerMoveParentRequest
         * @static
         * @param {IFilerMoveParentRequest=} [properties] Properties to set
         * @returns {FilerMoveParentRequest} FilerMoveParentRequest instance
         */
        FilerMoveParentRequest.create = function create(properties) {
            return new FilerMoveParentRequest(properties);
        };
    
        /**
         * Encodes the specified FilerMoveParentRequest message. Does not implicitly {@link FilerMoveParentRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerMoveParentRequest
         * @static
         * @param {IFilerMoveParentRequest} message FilerMoveParentRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveParentRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.name);
            return writer;
        };
    
        /**
         * Encodes the specified FilerMoveParentRequest message, length delimited. Does not implicitly {@link FilerMoveParentRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerMoveParentRequest
         * @static
         * @param {IFilerMoveParentRequest} message FilerMoveParentRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveParentRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerMoveParentRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerMoveParentRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerMoveParentRequest} FilerMoveParentRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveParentRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerMoveParentRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.name = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerMoveParentRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerMoveParentRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerMoveParentRequest} FilerMoveParentRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveParentRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerMoveParentRequest message.
         * @function verify
         * @memberof FilerMoveParentRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerMoveParentRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            return null;
        };
    
        /**
         * Creates a FilerMoveParentRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerMoveParentRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerMoveParentRequest} FilerMoveParentRequest
         */
        FilerMoveParentRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerMoveParentRequest)
                return object;
            var message = new $root.FilerMoveParentRequest();
            if (object.name != null)
                message.name = String(object.name);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerMoveParentRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerMoveParentRequest
         * @static
         * @param {FilerMoveParentRequest} message FilerMoveParentRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerMoveParentRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.name = "";
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            return object;
        };
    
        /**
         * Converts this FilerMoveParentRequest to JSON.
         * @function toJSON
         * @memberof FilerMoveParentRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerMoveParentRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerMoveParentRequest;
    })();
    
    $root.FilerMoveParentResponse = (function() {
    
        /**
         * Properties of a FilerMoveParentResponse.
         * @exports IFilerMoveParentResponse
         * @interface IFilerMoveParentResponse
         * @property {IFiler|null} [filer] FilerMoveParentResponse filer
         */
    
        /**
         * Constructs a new FilerMoveParentResponse.
         * @exports FilerMoveParentResponse
         * @classdesc Represents a FilerMoveParentResponse.
         * @implements IFilerMoveParentResponse
         * @constructor
         * @param {IFilerMoveParentResponse=} [properties] Properties to set
         */
        function FilerMoveParentResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerMoveParentResponse filer.
         * @member {IFiler|null|undefined} filer
         * @memberof FilerMoveParentResponse
         * @instance
         */
        FilerMoveParentResponse.prototype.filer = null;
    
        /**
         * Creates a new FilerMoveParentResponse instance using the specified properties.
         * @function create
         * @memberof FilerMoveParentResponse
         * @static
         * @param {IFilerMoveParentResponse=} [properties] Properties to set
         * @returns {FilerMoveParentResponse} FilerMoveParentResponse instance
         */
        FilerMoveParentResponse.create = function create(properties) {
            return new FilerMoveParentResponse(properties);
        };
    
        /**
         * Encodes the specified FilerMoveParentResponse message. Does not implicitly {@link FilerMoveParentResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerMoveParentResponse
         * @static
         * @param {IFilerMoveParentResponse} message FilerMoveParentResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveParentResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.filer != null && message.hasOwnProperty("filer"))
                $root.Filer.encode(message.filer, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FilerMoveParentResponse message, length delimited. Does not implicitly {@link FilerMoveParentResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerMoveParentResponse
         * @static
         * @param {IFilerMoveParentResponse} message FilerMoveParentResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveParentResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerMoveParentResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerMoveParentResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerMoveParentResponse} FilerMoveParentResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveParentResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerMoveParentResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.filer = $root.Filer.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerMoveParentResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerMoveParentResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerMoveParentResponse} FilerMoveParentResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveParentResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerMoveParentResponse message.
         * @function verify
         * @memberof FilerMoveParentResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerMoveParentResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.filer != null && message.hasOwnProperty("filer")) {
                var error = $root.Filer.verify(message.filer);
                if (error)
                    return "filer." + error;
            }
            return null;
        };
    
        /**
         * Creates a FilerMoveParentResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerMoveParentResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerMoveParentResponse} FilerMoveParentResponse
         */
        FilerMoveParentResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerMoveParentResponse)
                return object;
            var message = new $root.FilerMoveParentResponse();
            if (object.filer != null) {
                if (typeof object.filer !== "object")
                    throw TypeError(".FilerMoveParentResponse.filer: object expected");
                message.filer = $root.Filer.fromObject(object.filer);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerMoveParentResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerMoveParentResponse
         * @static
         * @param {FilerMoveParentResponse} message FilerMoveParentResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerMoveParentResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.filer = null;
            if (message.filer != null && message.hasOwnProperty("filer"))
                object.filer = $root.Filer.toObject(message.filer, options);
            return object;
        };
    
        /**
         * Converts this FilerMoveParentResponse to JSON.
         * @function toJSON
         * @memberof FilerMoveParentResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerMoveParentResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerMoveParentResponse;
    })();
    
    $root.FilerEnterDirectoryRequest = (function() {
    
        /**
         * Properties of a FilerEnterDirectoryRequest.
         * @exports IFilerEnterDirectoryRequest
         * @interface IFilerEnterDirectoryRequest
         * @property {string|null} [name] FilerEnterDirectoryRequest name
         * @property {string|null} [itemId] FilerEnterDirectoryRequest itemId
         */
    
        /**
         * Constructs a new FilerEnterDirectoryRequest.
         * @exports FilerEnterDirectoryRequest
         * @classdesc Represents a FilerEnterDirectoryRequest.
         * @implements IFilerEnterDirectoryRequest
         * @constructor
         * @param {IFilerEnterDirectoryRequest=} [properties] Properties to set
         */
        function FilerEnterDirectoryRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerEnterDirectoryRequest name.
         * @member {string} name
         * @memberof FilerEnterDirectoryRequest
         * @instance
         */
        FilerEnterDirectoryRequest.prototype.name = "";
    
        /**
         * FilerEnterDirectoryRequest itemId.
         * @member {string} itemId
         * @memberof FilerEnterDirectoryRequest
         * @instance
         */
        FilerEnterDirectoryRequest.prototype.itemId = "";
    
        /**
         * Creates a new FilerEnterDirectoryRequest instance using the specified properties.
         * @function create
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {IFilerEnterDirectoryRequest=} [properties] Properties to set
         * @returns {FilerEnterDirectoryRequest} FilerEnterDirectoryRequest instance
         */
        FilerEnterDirectoryRequest.create = function create(properties) {
            return new FilerEnterDirectoryRequest(properties);
        };
    
        /**
         * Encodes the specified FilerEnterDirectoryRequest message. Does not implicitly {@link FilerEnterDirectoryRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {IFilerEnterDirectoryRequest} message FilerEnterDirectoryRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerEnterDirectoryRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.name);
            if (message.itemId != null && message.hasOwnProperty("itemId"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.itemId);
            return writer;
        };
    
        /**
         * Encodes the specified FilerEnterDirectoryRequest message, length delimited. Does not implicitly {@link FilerEnterDirectoryRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {IFilerEnterDirectoryRequest} message FilerEnterDirectoryRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerEnterDirectoryRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerEnterDirectoryRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerEnterDirectoryRequest} FilerEnterDirectoryRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerEnterDirectoryRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerEnterDirectoryRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.name = reader.string();
                    break;
                case 2:
                    message.itemId = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerEnterDirectoryRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerEnterDirectoryRequest} FilerEnterDirectoryRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerEnterDirectoryRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerEnterDirectoryRequest message.
         * @function verify
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerEnterDirectoryRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            if (message.itemId != null && message.hasOwnProperty("itemId"))
                if (!$util.isString(message.itemId))
                    return "itemId: string expected";
            return null;
        };
    
        /**
         * Creates a FilerEnterDirectoryRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerEnterDirectoryRequest} FilerEnterDirectoryRequest
         */
        FilerEnterDirectoryRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerEnterDirectoryRequest)
                return object;
            var message = new $root.FilerEnterDirectoryRequest();
            if (object.name != null)
                message.name = String(object.name);
            if (object.itemId != null)
                message.itemId = String(object.itemId);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerEnterDirectoryRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerEnterDirectoryRequest
         * @static
         * @param {FilerEnterDirectoryRequest} message FilerEnterDirectoryRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerEnterDirectoryRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.name = "";
                object.itemId = "";
            }
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            if (message.itemId != null && message.hasOwnProperty("itemId"))
                object.itemId = message.itemId;
            return object;
        };
    
        /**
         * Converts this FilerEnterDirectoryRequest to JSON.
         * @function toJSON
         * @memberof FilerEnterDirectoryRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerEnterDirectoryRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerEnterDirectoryRequest;
    })();
    
    $root.FilerEnterDirectoryResponse = (function() {
    
        /**
         * Properties of a FilerEnterDirectoryResponse.
         * @exports IFilerEnterDirectoryResponse
         * @interface IFilerEnterDirectoryResponse
         * @property {IFiler|null} [filer] FilerEnterDirectoryResponse filer
         */
    
        /**
         * Constructs a new FilerEnterDirectoryResponse.
         * @exports FilerEnterDirectoryResponse
         * @classdesc Represents a FilerEnterDirectoryResponse.
         * @implements IFilerEnterDirectoryResponse
         * @constructor
         * @param {IFilerEnterDirectoryResponse=} [properties] Properties to set
         */
        function FilerEnterDirectoryResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerEnterDirectoryResponse filer.
         * @member {IFiler|null|undefined} filer
         * @memberof FilerEnterDirectoryResponse
         * @instance
         */
        FilerEnterDirectoryResponse.prototype.filer = null;
    
        /**
         * Creates a new FilerEnterDirectoryResponse instance using the specified properties.
         * @function create
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {IFilerEnterDirectoryResponse=} [properties] Properties to set
         * @returns {FilerEnterDirectoryResponse} FilerEnterDirectoryResponse instance
         */
        FilerEnterDirectoryResponse.create = function create(properties) {
            return new FilerEnterDirectoryResponse(properties);
        };
    
        /**
         * Encodes the specified FilerEnterDirectoryResponse message. Does not implicitly {@link FilerEnterDirectoryResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {IFilerEnterDirectoryResponse} message FilerEnterDirectoryResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerEnterDirectoryResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.filer != null && message.hasOwnProperty("filer"))
                $root.Filer.encode(message.filer, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FilerEnterDirectoryResponse message, length delimited. Does not implicitly {@link FilerEnterDirectoryResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {IFilerEnterDirectoryResponse} message FilerEnterDirectoryResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerEnterDirectoryResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerEnterDirectoryResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerEnterDirectoryResponse} FilerEnterDirectoryResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerEnterDirectoryResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerEnterDirectoryResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.filer = $root.Filer.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerEnterDirectoryResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerEnterDirectoryResponse} FilerEnterDirectoryResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerEnterDirectoryResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerEnterDirectoryResponse message.
         * @function verify
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerEnterDirectoryResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.filer != null && message.hasOwnProperty("filer")) {
                var error = $root.Filer.verify(message.filer);
                if (error)
                    return "filer." + error;
            }
            return null;
        };
    
        /**
         * Creates a FilerEnterDirectoryResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerEnterDirectoryResponse} FilerEnterDirectoryResponse
         */
        FilerEnterDirectoryResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerEnterDirectoryResponse)
                return object;
            var message = new $root.FilerEnterDirectoryResponse();
            if (object.filer != null) {
                if (typeof object.filer !== "object")
                    throw TypeError(".FilerEnterDirectoryResponse.filer: object expected");
                message.filer = $root.Filer.fromObject(object.filer);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerEnterDirectoryResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerEnterDirectoryResponse
         * @static
         * @param {FilerEnterDirectoryResponse} message FilerEnterDirectoryResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerEnterDirectoryResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.filer = null;
            if (message.filer != null && message.hasOwnProperty("filer"))
                object.filer = $root.Filer.toObject(message.filer, options);
            return object;
        };
    
        /**
         * Converts this FilerEnterDirectoryResponse to JSON.
         * @function toJSON
         * @memberof FilerEnterDirectoryResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerEnterDirectoryResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerEnterDirectoryResponse;
    })();
    
    $root.FilerToggleMarkRequest = (function() {
    
        /**
         * Properties of a FilerToggleMarkRequest.
         * @exports IFilerToggleMarkRequest
         * @interface IFilerToggleMarkRequest
         * @property {string|null} [name] FilerToggleMarkRequest name
         * @property {Array.<string>|null} [itemIds] FilerToggleMarkRequest itemIds
         */
    
        /**
         * Constructs a new FilerToggleMarkRequest.
         * @exports FilerToggleMarkRequest
         * @classdesc Represents a FilerToggleMarkRequest.
         * @implements IFilerToggleMarkRequest
         * @constructor
         * @param {IFilerToggleMarkRequest=} [properties] Properties to set
         */
        function FilerToggleMarkRequest(properties) {
            this.itemIds = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerToggleMarkRequest name.
         * @member {string} name
         * @memberof FilerToggleMarkRequest
         * @instance
         */
        FilerToggleMarkRequest.prototype.name = "";
    
        /**
         * FilerToggleMarkRequest itemIds.
         * @member {Array.<string>} itemIds
         * @memberof FilerToggleMarkRequest
         * @instance
         */
        FilerToggleMarkRequest.prototype.itemIds = $util.emptyArray;
    
        /**
         * Creates a new FilerToggleMarkRequest instance using the specified properties.
         * @function create
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {IFilerToggleMarkRequest=} [properties] Properties to set
         * @returns {FilerToggleMarkRequest} FilerToggleMarkRequest instance
         */
        FilerToggleMarkRequest.create = function create(properties) {
            return new FilerToggleMarkRequest(properties);
        };
    
        /**
         * Encodes the specified FilerToggleMarkRequest message. Does not implicitly {@link FilerToggleMarkRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {IFilerToggleMarkRequest} message FilerToggleMarkRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerToggleMarkRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.name);
            if (message.itemIds != null && message.itemIds.length)
                for (var i = 0; i < message.itemIds.length; ++i)
                    writer.uint32(/* id 2, wireType 2 =*/18).string(message.itemIds[i]);
            return writer;
        };
    
        /**
         * Encodes the specified FilerToggleMarkRequest message, length delimited. Does not implicitly {@link FilerToggleMarkRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {IFilerToggleMarkRequest} message FilerToggleMarkRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerToggleMarkRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerToggleMarkRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerToggleMarkRequest} FilerToggleMarkRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerToggleMarkRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerToggleMarkRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.name = reader.string();
                    break;
                case 2:
                    if (!(message.itemIds && message.itemIds.length))
                        message.itemIds = [];
                    message.itemIds.push(reader.string());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerToggleMarkRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerToggleMarkRequest} FilerToggleMarkRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerToggleMarkRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerToggleMarkRequest message.
         * @function verify
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerToggleMarkRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            if (message.itemIds != null && message.hasOwnProperty("itemIds")) {
                if (!Array.isArray(message.itemIds))
                    return "itemIds: array expected";
                for (var i = 0; i < message.itemIds.length; ++i)
                    if (!$util.isString(message.itemIds[i]))
                        return "itemIds: string[] expected";
            }
            return null;
        };
    
        /**
         * Creates a FilerToggleMarkRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerToggleMarkRequest} FilerToggleMarkRequest
         */
        FilerToggleMarkRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerToggleMarkRequest)
                return object;
            var message = new $root.FilerToggleMarkRequest();
            if (object.name != null)
                message.name = String(object.name);
            if (object.itemIds) {
                if (!Array.isArray(object.itemIds))
                    throw TypeError(".FilerToggleMarkRequest.itemIds: array expected");
                message.itemIds = [];
                for (var i = 0; i < object.itemIds.length; ++i)
                    message.itemIds[i] = String(object.itemIds[i]);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerToggleMarkRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerToggleMarkRequest
         * @static
         * @param {FilerToggleMarkRequest} message FilerToggleMarkRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerToggleMarkRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.itemIds = [];
            if (options.defaults)
                object.name = "";
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            if (message.itemIds && message.itemIds.length) {
                object.itemIds = [];
                for (var j = 0; j < message.itemIds.length; ++j)
                    object.itemIds[j] = message.itemIds[j];
            }
            return object;
        };
    
        /**
         * Converts this FilerToggleMarkRequest to JSON.
         * @function toJSON
         * @memberof FilerToggleMarkRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerToggleMarkRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerToggleMarkRequest;
    })();
    
    $root.FilerToggleMarkResponse = (function() {
    
        /**
         * Properties of a FilerToggleMarkResponse.
         * @exports IFilerToggleMarkResponse
         * @interface IFilerToggleMarkResponse
         * @property {IFiler|null} [filer] FilerToggleMarkResponse filer
         */
    
        /**
         * Constructs a new FilerToggleMarkResponse.
         * @exports FilerToggleMarkResponse
         * @classdesc Represents a FilerToggleMarkResponse.
         * @implements IFilerToggleMarkResponse
         * @constructor
         * @param {IFilerToggleMarkResponse=} [properties] Properties to set
         */
        function FilerToggleMarkResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerToggleMarkResponse filer.
         * @member {IFiler|null|undefined} filer
         * @memberof FilerToggleMarkResponse
         * @instance
         */
        FilerToggleMarkResponse.prototype.filer = null;
    
        /**
         * Creates a new FilerToggleMarkResponse instance using the specified properties.
         * @function create
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {IFilerToggleMarkResponse=} [properties] Properties to set
         * @returns {FilerToggleMarkResponse} FilerToggleMarkResponse instance
         */
        FilerToggleMarkResponse.create = function create(properties) {
            return new FilerToggleMarkResponse(properties);
        };
    
        /**
         * Encodes the specified FilerToggleMarkResponse message. Does not implicitly {@link FilerToggleMarkResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {IFilerToggleMarkResponse} message FilerToggleMarkResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerToggleMarkResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.filer != null && message.hasOwnProperty("filer"))
                $root.Filer.encode(message.filer, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FilerToggleMarkResponse message, length delimited. Does not implicitly {@link FilerToggleMarkResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {IFilerToggleMarkResponse} message FilerToggleMarkResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerToggleMarkResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerToggleMarkResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerToggleMarkResponse} FilerToggleMarkResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerToggleMarkResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerToggleMarkResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.filer = $root.Filer.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerToggleMarkResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerToggleMarkResponse} FilerToggleMarkResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerToggleMarkResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerToggleMarkResponse message.
         * @function verify
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerToggleMarkResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.filer != null && message.hasOwnProperty("filer")) {
                var error = $root.Filer.verify(message.filer);
                if (error)
                    return "filer." + error;
            }
            return null;
        };
    
        /**
         * Creates a FilerToggleMarkResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerToggleMarkResponse} FilerToggleMarkResponse
         */
        FilerToggleMarkResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerToggleMarkResponse)
                return object;
            var message = new $root.FilerToggleMarkResponse();
            if (object.filer != null) {
                if (typeof object.filer !== "object")
                    throw TypeError(".FilerToggleMarkResponse.filer: object expected");
                message.filer = $root.Filer.fromObject(object.filer);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerToggleMarkResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerToggleMarkResponse
         * @static
         * @param {FilerToggleMarkResponse} message FilerToggleMarkResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerToggleMarkResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.filer = null;
            if (message.filer != null && message.hasOwnProperty("filer"))
                object.filer = $root.Filer.toObject(message.filer, options);
            return object;
        };
    
        /**
         * Converts this FilerToggleMarkResponse to JSON.
         * @function toJSON
         * @memberof FilerToggleMarkResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerToggleMarkResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerToggleMarkResponse;
    })();
    
    $root.FilerMoveRequest = (function() {
    
        /**
         * Properties of a FilerMoveRequest.
         * @exports IFilerMoveRequest
         * @interface IFilerMoveRequest
         * @property {string|null} [source] FilerMoveRequest source
         * @property {string|null} [dest] FilerMoveRequest dest
         * @property {Array.<string>|null} [itemIds] FilerMoveRequest itemIds
         */
    
        /**
         * Constructs a new FilerMoveRequest.
         * @exports FilerMoveRequest
         * @classdesc Represents a FilerMoveRequest.
         * @implements IFilerMoveRequest
         * @constructor
         * @param {IFilerMoveRequest=} [properties] Properties to set
         */
        function FilerMoveRequest(properties) {
            this.itemIds = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerMoveRequest source.
         * @member {string} source
         * @memberof FilerMoveRequest
         * @instance
         */
        FilerMoveRequest.prototype.source = "";
    
        /**
         * FilerMoveRequest dest.
         * @member {string} dest
         * @memberof FilerMoveRequest
         * @instance
         */
        FilerMoveRequest.prototype.dest = "";
    
        /**
         * FilerMoveRequest itemIds.
         * @member {Array.<string>} itemIds
         * @memberof FilerMoveRequest
         * @instance
         */
        FilerMoveRequest.prototype.itemIds = $util.emptyArray;
    
        /**
         * Creates a new FilerMoveRequest instance using the specified properties.
         * @function create
         * @memberof FilerMoveRequest
         * @static
         * @param {IFilerMoveRequest=} [properties] Properties to set
         * @returns {FilerMoveRequest} FilerMoveRequest instance
         */
        FilerMoveRequest.create = function create(properties) {
            return new FilerMoveRequest(properties);
        };
    
        /**
         * Encodes the specified FilerMoveRequest message. Does not implicitly {@link FilerMoveRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerMoveRequest
         * @static
         * @param {IFilerMoveRequest} message FilerMoveRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.source != null && message.hasOwnProperty("source"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.source);
            if (message.dest != null && message.hasOwnProperty("dest"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.dest);
            if (message.itemIds != null && message.itemIds.length)
                for (var i = 0; i < message.itemIds.length; ++i)
                    writer.uint32(/* id 3, wireType 2 =*/26).string(message.itemIds[i]);
            return writer;
        };
    
        /**
         * Encodes the specified FilerMoveRequest message, length delimited. Does not implicitly {@link FilerMoveRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerMoveRequest
         * @static
         * @param {IFilerMoveRequest} message FilerMoveRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerMoveRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerMoveRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerMoveRequest} FilerMoveRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerMoveRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.source = reader.string();
                    break;
                case 2:
                    message.dest = reader.string();
                    break;
                case 3:
                    if (!(message.itemIds && message.itemIds.length))
                        message.itemIds = [];
                    message.itemIds.push(reader.string());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerMoveRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerMoveRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerMoveRequest} FilerMoveRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerMoveRequest message.
         * @function verify
         * @memberof FilerMoveRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerMoveRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.source != null && message.hasOwnProperty("source"))
                if (!$util.isString(message.source))
                    return "source: string expected";
            if (message.dest != null && message.hasOwnProperty("dest"))
                if (!$util.isString(message.dest))
                    return "dest: string expected";
            if (message.itemIds != null && message.hasOwnProperty("itemIds")) {
                if (!Array.isArray(message.itemIds))
                    return "itemIds: array expected";
                for (var i = 0; i < message.itemIds.length; ++i)
                    if (!$util.isString(message.itemIds[i]))
                        return "itemIds: string[] expected";
            }
            return null;
        };
    
        /**
         * Creates a FilerMoveRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerMoveRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerMoveRequest} FilerMoveRequest
         */
        FilerMoveRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerMoveRequest)
                return object;
            var message = new $root.FilerMoveRequest();
            if (object.source != null)
                message.source = String(object.source);
            if (object.dest != null)
                message.dest = String(object.dest);
            if (object.itemIds) {
                if (!Array.isArray(object.itemIds))
                    throw TypeError(".FilerMoveRequest.itemIds: array expected");
                message.itemIds = [];
                for (var i = 0; i < object.itemIds.length; ++i)
                    message.itemIds[i] = String(object.itemIds[i]);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerMoveRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerMoveRequest
         * @static
         * @param {FilerMoveRequest} message FilerMoveRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerMoveRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.itemIds = [];
            if (options.defaults) {
                object.source = "";
                object.dest = "";
            }
            if (message.source != null && message.hasOwnProperty("source"))
                object.source = message.source;
            if (message.dest != null && message.hasOwnProperty("dest"))
                object.dest = message.dest;
            if (message.itemIds && message.itemIds.length) {
                object.itemIds = [];
                for (var j = 0; j < message.itemIds.length; ++j)
                    object.itemIds[j] = message.itemIds[j];
            }
            return object;
        };
    
        /**
         * Converts this FilerMoveRequest to JSON.
         * @function toJSON
         * @memberof FilerMoveRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerMoveRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerMoveRequest;
    })();
    
    $root.FilerMoveResponse = (function() {
    
        /**
         * Properties of a FilerMoveResponse.
         * @exports IFilerMoveResponse
         * @interface IFilerMoveResponse
         * @property {string|null} [taskId] FilerMoveResponse taskId
         * @property {string|null} [taskName] FilerMoveResponse taskName
         */
    
        /**
         * Constructs a new FilerMoveResponse.
         * @exports FilerMoveResponse
         * @classdesc Represents a FilerMoveResponse.
         * @implements IFilerMoveResponse
         * @constructor
         * @param {IFilerMoveResponse=} [properties] Properties to set
         */
        function FilerMoveResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerMoveResponse taskId.
         * @member {string} taskId
         * @memberof FilerMoveResponse
         * @instance
         */
        FilerMoveResponse.prototype.taskId = "";
    
        /**
         * FilerMoveResponse taskName.
         * @member {string} taskName
         * @memberof FilerMoveResponse
         * @instance
         */
        FilerMoveResponse.prototype.taskName = "";
    
        /**
         * Creates a new FilerMoveResponse instance using the specified properties.
         * @function create
         * @memberof FilerMoveResponse
         * @static
         * @param {IFilerMoveResponse=} [properties] Properties to set
         * @returns {FilerMoveResponse} FilerMoveResponse instance
         */
        FilerMoveResponse.create = function create(properties) {
            return new FilerMoveResponse(properties);
        };
    
        /**
         * Encodes the specified FilerMoveResponse message. Does not implicitly {@link FilerMoveResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerMoveResponse
         * @static
         * @param {IFilerMoveResponse} message FilerMoveResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.taskId);
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.taskName);
            return writer;
        };
    
        /**
         * Encodes the specified FilerMoveResponse message, length delimited. Does not implicitly {@link FilerMoveResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerMoveResponse
         * @static
         * @param {IFilerMoveResponse} message FilerMoveResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerMoveResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerMoveResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerMoveResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerMoveResponse} FilerMoveResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerMoveResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.taskId = reader.string();
                    break;
                case 2:
                    message.taskName = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerMoveResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerMoveResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerMoveResponse} FilerMoveResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerMoveResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerMoveResponse message.
         * @function verify
         * @memberof FilerMoveResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerMoveResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                if (!$util.isString(message.taskName))
                    return "taskName: string expected";
            return null;
        };
    
        /**
         * Creates a FilerMoveResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerMoveResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerMoveResponse} FilerMoveResponse
         */
        FilerMoveResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerMoveResponse)
                return object;
            var message = new $root.FilerMoveResponse();
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            if (object.taskName != null)
                message.taskName = String(object.taskName);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerMoveResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerMoveResponse
         * @static
         * @param {FilerMoveResponse} message FilerMoveResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerMoveResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.taskId = "";
                object.taskName = "";
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                object.taskName = message.taskName;
            return object;
        };
    
        /**
         * Converts this FilerMoveResponse to JSON.
         * @function toJSON
         * @memberof FilerMoveResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerMoveResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerMoveResponse;
    })();
    
    $root.FilerDeleteRequest = (function() {
    
        /**
         * Properties of a FilerDeleteRequest.
         * @exports IFilerDeleteRequest
         * @interface IFilerDeleteRequest
         * @property {string|null} [source] FilerDeleteRequest source
         * @property {Array.<string>|null} [itemIds] FilerDeleteRequest itemIds
         */
    
        /**
         * Constructs a new FilerDeleteRequest.
         * @exports FilerDeleteRequest
         * @classdesc Represents a FilerDeleteRequest.
         * @implements IFilerDeleteRequest
         * @constructor
         * @param {IFilerDeleteRequest=} [properties] Properties to set
         */
        function FilerDeleteRequest(properties) {
            this.itemIds = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerDeleteRequest source.
         * @member {string} source
         * @memberof FilerDeleteRequest
         * @instance
         */
        FilerDeleteRequest.prototype.source = "";
    
        /**
         * FilerDeleteRequest itemIds.
         * @member {Array.<string>} itemIds
         * @memberof FilerDeleteRequest
         * @instance
         */
        FilerDeleteRequest.prototype.itemIds = $util.emptyArray;
    
        /**
         * Creates a new FilerDeleteRequest instance using the specified properties.
         * @function create
         * @memberof FilerDeleteRequest
         * @static
         * @param {IFilerDeleteRequest=} [properties] Properties to set
         * @returns {FilerDeleteRequest} FilerDeleteRequest instance
         */
        FilerDeleteRequest.create = function create(properties) {
            return new FilerDeleteRequest(properties);
        };
    
        /**
         * Encodes the specified FilerDeleteRequest message. Does not implicitly {@link FilerDeleteRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerDeleteRequest
         * @static
         * @param {IFilerDeleteRequest} message FilerDeleteRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerDeleteRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.source != null && message.hasOwnProperty("source"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.source);
            if (message.itemIds != null && message.itemIds.length)
                for (var i = 0; i < message.itemIds.length; ++i)
                    writer.uint32(/* id 2, wireType 2 =*/18).string(message.itemIds[i]);
            return writer;
        };
    
        /**
         * Encodes the specified FilerDeleteRequest message, length delimited. Does not implicitly {@link FilerDeleteRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerDeleteRequest
         * @static
         * @param {IFilerDeleteRequest} message FilerDeleteRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerDeleteRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerDeleteRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerDeleteRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerDeleteRequest} FilerDeleteRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerDeleteRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerDeleteRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.source = reader.string();
                    break;
                case 2:
                    if (!(message.itemIds && message.itemIds.length))
                        message.itemIds = [];
                    message.itemIds.push(reader.string());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerDeleteRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerDeleteRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerDeleteRequest} FilerDeleteRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerDeleteRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerDeleteRequest message.
         * @function verify
         * @memberof FilerDeleteRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerDeleteRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.source != null && message.hasOwnProperty("source"))
                if (!$util.isString(message.source))
                    return "source: string expected";
            if (message.itemIds != null && message.hasOwnProperty("itemIds")) {
                if (!Array.isArray(message.itemIds))
                    return "itemIds: array expected";
                for (var i = 0; i < message.itemIds.length; ++i)
                    if (!$util.isString(message.itemIds[i]))
                        return "itemIds: string[] expected";
            }
            return null;
        };
    
        /**
         * Creates a FilerDeleteRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerDeleteRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerDeleteRequest} FilerDeleteRequest
         */
        FilerDeleteRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerDeleteRequest)
                return object;
            var message = new $root.FilerDeleteRequest();
            if (object.source != null)
                message.source = String(object.source);
            if (object.itemIds) {
                if (!Array.isArray(object.itemIds))
                    throw TypeError(".FilerDeleteRequest.itemIds: array expected");
                message.itemIds = [];
                for (var i = 0; i < object.itemIds.length; ++i)
                    message.itemIds[i] = String(object.itemIds[i]);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerDeleteRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerDeleteRequest
         * @static
         * @param {FilerDeleteRequest} message FilerDeleteRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerDeleteRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.itemIds = [];
            if (options.defaults)
                object.source = "";
            if (message.source != null && message.hasOwnProperty("source"))
                object.source = message.source;
            if (message.itemIds && message.itemIds.length) {
                object.itemIds = [];
                for (var j = 0; j < message.itemIds.length; ++j)
                    object.itemIds[j] = message.itemIds[j];
            }
            return object;
        };
    
        /**
         * Converts this FilerDeleteRequest to JSON.
         * @function toJSON
         * @memberof FilerDeleteRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerDeleteRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerDeleteRequest;
    })();
    
    $root.FilerDeleteResponse = (function() {
    
        /**
         * Properties of a FilerDeleteResponse.
         * @exports IFilerDeleteResponse
         * @interface IFilerDeleteResponse
         * @property {string|null} [taskId] FilerDeleteResponse taskId
         * @property {string|null} [taskName] FilerDeleteResponse taskName
         */
    
        /**
         * Constructs a new FilerDeleteResponse.
         * @exports FilerDeleteResponse
         * @classdesc Represents a FilerDeleteResponse.
         * @implements IFilerDeleteResponse
         * @constructor
         * @param {IFilerDeleteResponse=} [properties] Properties to set
         */
        function FilerDeleteResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerDeleteResponse taskId.
         * @member {string} taskId
         * @memberof FilerDeleteResponse
         * @instance
         */
        FilerDeleteResponse.prototype.taskId = "";
    
        /**
         * FilerDeleteResponse taskName.
         * @member {string} taskName
         * @memberof FilerDeleteResponse
         * @instance
         */
        FilerDeleteResponse.prototype.taskName = "";
    
        /**
         * Creates a new FilerDeleteResponse instance using the specified properties.
         * @function create
         * @memberof FilerDeleteResponse
         * @static
         * @param {IFilerDeleteResponse=} [properties] Properties to set
         * @returns {FilerDeleteResponse} FilerDeleteResponse instance
         */
        FilerDeleteResponse.create = function create(properties) {
            return new FilerDeleteResponse(properties);
        };
    
        /**
         * Encodes the specified FilerDeleteResponse message. Does not implicitly {@link FilerDeleteResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerDeleteResponse
         * @static
         * @param {IFilerDeleteResponse} message FilerDeleteResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerDeleteResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.taskId);
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.taskName);
            return writer;
        };
    
        /**
         * Encodes the specified FilerDeleteResponse message, length delimited. Does not implicitly {@link FilerDeleteResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerDeleteResponse
         * @static
         * @param {IFilerDeleteResponse} message FilerDeleteResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerDeleteResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerDeleteResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerDeleteResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerDeleteResponse} FilerDeleteResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerDeleteResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerDeleteResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.taskId = reader.string();
                    break;
                case 2:
                    message.taskName = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerDeleteResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerDeleteResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerDeleteResponse} FilerDeleteResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerDeleteResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerDeleteResponse message.
         * @function verify
         * @memberof FilerDeleteResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerDeleteResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                if (!$util.isString(message.taskName))
                    return "taskName: string expected";
            return null;
        };
    
        /**
         * Creates a FilerDeleteResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerDeleteResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerDeleteResponse} FilerDeleteResponse
         */
        FilerDeleteResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerDeleteResponse)
                return object;
            var message = new $root.FilerDeleteResponse();
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            if (object.taskName != null)
                message.taskName = String(object.taskName);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerDeleteResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerDeleteResponse
         * @static
         * @param {FilerDeleteResponse} message FilerDeleteResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerDeleteResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.taskId = "";
                object.taskName = "";
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                object.taskName = message.taskName;
            return object;
        };
    
        /**
         * Converts this FilerDeleteResponse to JSON.
         * @function toJSON
         * @memberof FilerDeleteResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerDeleteResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerDeleteResponse;
    })();
    
    $root.FilerCopyRequest = (function() {
    
        /**
         * Properties of a FilerCopyRequest.
         * @exports IFilerCopyRequest
         * @interface IFilerCopyRequest
         * @property {string|null} [source] FilerCopyRequest source
         * @property {string|null} [dest] FilerCopyRequest dest
         * @property {Array.<string>|null} [itemIds] FilerCopyRequest itemIds
         */
    
        /**
         * Constructs a new FilerCopyRequest.
         * @exports FilerCopyRequest
         * @classdesc Represents a FilerCopyRequest.
         * @implements IFilerCopyRequest
         * @constructor
         * @param {IFilerCopyRequest=} [properties] Properties to set
         */
        function FilerCopyRequest(properties) {
            this.itemIds = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerCopyRequest source.
         * @member {string} source
         * @memberof FilerCopyRequest
         * @instance
         */
        FilerCopyRequest.prototype.source = "";
    
        /**
         * FilerCopyRequest dest.
         * @member {string} dest
         * @memberof FilerCopyRequest
         * @instance
         */
        FilerCopyRequest.prototype.dest = "";
    
        /**
         * FilerCopyRequest itemIds.
         * @member {Array.<string>} itemIds
         * @memberof FilerCopyRequest
         * @instance
         */
        FilerCopyRequest.prototype.itemIds = $util.emptyArray;
    
        /**
         * Creates a new FilerCopyRequest instance using the specified properties.
         * @function create
         * @memberof FilerCopyRequest
         * @static
         * @param {IFilerCopyRequest=} [properties] Properties to set
         * @returns {FilerCopyRequest} FilerCopyRequest instance
         */
        FilerCopyRequest.create = function create(properties) {
            return new FilerCopyRequest(properties);
        };
    
        /**
         * Encodes the specified FilerCopyRequest message. Does not implicitly {@link FilerCopyRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerCopyRequest
         * @static
         * @param {IFilerCopyRequest} message FilerCopyRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerCopyRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.source != null && message.hasOwnProperty("source"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.source);
            if (message.dest != null && message.hasOwnProperty("dest"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.dest);
            if (message.itemIds != null && message.itemIds.length)
                for (var i = 0; i < message.itemIds.length; ++i)
                    writer.uint32(/* id 3, wireType 2 =*/26).string(message.itemIds[i]);
            return writer;
        };
    
        /**
         * Encodes the specified FilerCopyRequest message, length delimited. Does not implicitly {@link FilerCopyRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerCopyRequest
         * @static
         * @param {IFilerCopyRequest} message FilerCopyRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerCopyRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerCopyRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerCopyRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerCopyRequest} FilerCopyRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerCopyRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerCopyRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.source = reader.string();
                    break;
                case 2:
                    message.dest = reader.string();
                    break;
                case 3:
                    if (!(message.itemIds && message.itemIds.length))
                        message.itemIds = [];
                    message.itemIds.push(reader.string());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerCopyRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerCopyRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerCopyRequest} FilerCopyRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerCopyRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerCopyRequest message.
         * @function verify
         * @memberof FilerCopyRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerCopyRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.source != null && message.hasOwnProperty("source"))
                if (!$util.isString(message.source))
                    return "source: string expected";
            if (message.dest != null && message.hasOwnProperty("dest"))
                if (!$util.isString(message.dest))
                    return "dest: string expected";
            if (message.itemIds != null && message.hasOwnProperty("itemIds")) {
                if (!Array.isArray(message.itemIds))
                    return "itemIds: array expected";
                for (var i = 0; i < message.itemIds.length; ++i)
                    if (!$util.isString(message.itemIds[i]))
                        return "itemIds: string[] expected";
            }
            return null;
        };
    
        /**
         * Creates a FilerCopyRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerCopyRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerCopyRequest} FilerCopyRequest
         */
        FilerCopyRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerCopyRequest)
                return object;
            var message = new $root.FilerCopyRequest();
            if (object.source != null)
                message.source = String(object.source);
            if (object.dest != null)
                message.dest = String(object.dest);
            if (object.itemIds) {
                if (!Array.isArray(object.itemIds))
                    throw TypeError(".FilerCopyRequest.itemIds: array expected");
                message.itemIds = [];
                for (var i = 0; i < object.itemIds.length; ++i)
                    message.itemIds[i] = String(object.itemIds[i]);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerCopyRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerCopyRequest
         * @static
         * @param {FilerCopyRequest} message FilerCopyRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerCopyRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.itemIds = [];
            if (options.defaults) {
                object.source = "";
                object.dest = "";
            }
            if (message.source != null && message.hasOwnProperty("source"))
                object.source = message.source;
            if (message.dest != null && message.hasOwnProperty("dest"))
                object.dest = message.dest;
            if (message.itemIds && message.itemIds.length) {
                object.itemIds = [];
                for (var j = 0; j < message.itemIds.length; ++j)
                    object.itemIds[j] = message.itemIds[j];
            }
            return object;
        };
    
        /**
         * Converts this FilerCopyRequest to JSON.
         * @function toJSON
         * @memberof FilerCopyRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerCopyRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerCopyRequest;
    })();
    
    $root.FilerCopyResponse = (function() {
    
        /**
         * Properties of a FilerCopyResponse.
         * @exports IFilerCopyResponse
         * @interface IFilerCopyResponse
         * @property {string|null} [taskId] FilerCopyResponse taskId
         * @property {string|null} [taskName] FilerCopyResponse taskName
         */
    
        /**
         * Constructs a new FilerCopyResponse.
         * @exports FilerCopyResponse
         * @classdesc Represents a FilerCopyResponse.
         * @implements IFilerCopyResponse
         * @constructor
         * @param {IFilerCopyResponse=} [properties] Properties to set
         */
        function FilerCopyResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerCopyResponse taskId.
         * @member {string} taskId
         * @memberof FilerCopyResponse
         * @instance
         */
        FilerCopyResponse.prototype.taskId = "";
    
        /**
         * FilerCopyResponse taskName.
         * @member {string} taskName
         * @memberof FilerCopyResponse
         * @instance
         */
        FilerCopyResponse.prototype.taskName = "";
    
        /**
         * Creates a new FilerCopyResponse instance using the specified properties.
         * @function create
         * @memberof FilerCopyResponse
         * @static
         * @param {IFilerCopyResponse=} [properties] Properties to set
         * @returns {FilerCopyResponse} FilerCopyResponse instance
         */
        FilerCopyResponse.create = function create(properties) {
            return new FilerCopyResponse(properties);
        };
    
        /**
         * Encodes the specified FilerCopyResponse message. Does not implicitly {@link FilerCopyResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerCopyResponse
         * @static
         * @param {IFilerCopyResponse} message FilerCopyResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerCopyResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.taskId);
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.taskName);
            return writer;
        };
    
        /**
         * Encodes the specified FilerCopyResponse message, length delimited. Does not implicitly {@link FilerCopyResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerCopyResponse
         * @static
         * @param {IFilerCopyResponse} message FilerCopyResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerCopyResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerCopyResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerCopyResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerCopyResponse} FilerCopyResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerCopyResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerCopyResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.taskId = reader.string();
                    break;
                case 2:
                    message.taskName = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerCopyResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerCopyResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerCopyResponse} FilerCopyResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerCopyResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerCopyResponse message.
         * @function verify
         * @memberof FilerCopyResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerCopyResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                if (!$util.isString(message.taskName))
                    return "taskName: string expected";
            return null;
        };
    
        /**
         * Creates a FilerCopyResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerCopyResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerCopyResponse} FilerCopyResponse
         */
        FilerCopyResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerCopyResponse)
                return object;
            var message = new $root.FilerCopyResponse();
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            if (object.taskName != null)
                message.taskName = String(object.taskName);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerCopyResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerCopyResponse
         * @static
         * @param {FilerCopyResponse} message FilerCopyResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerCopyResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.taskId = "";
                object.taskName = "";
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            if (message.taskName != null && message.hasOwnProperty("taskName"))
                object.taskName = message.taskName;
            return object;
        };
    
        /**
         * Converts this FilerCopyResponse to JSON.
         * @function toJSON
         * @memberof FilerCopyResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerCopyResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerCopyResponse;
    })();
    
    $root.FilerJumpLocationRequest = (function() {
    
        /**
         * Properties of a FilerJumpLocationRequest.
         * @exports IFilerJumpLocationRequest
         * @interface IFilerJumpLocationRequest
         * @property {string|null} [location] FilerJumpLocationRequest location
         * @property {string|null} [name] FilerJumpLocationRequest name
         */
    
        /**
         * Constructs a new FilerJumpLocationRequest.
         * @exports FilerJumpLocationRequest
         * @classdesc Represents a FilerJumpLocationRequest.
         * @implements IFilerJumpLocationRequest
         * @constructor
         * @param {IFilerJumpLocationRequest=} [properties] Properties to set
         */
        function FilerJumpLocationRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerJumpLocationRequest location.
         * @member {string} location
         * @memberof FilerJumpLocationRequest
         * @instance
         */
        FilerJumpLocationRequest.prototype.location = "";
    
        /**
         * FilerJumpLocationRequest name.
         * @member {string} name
         * @memberof FilerJumpLocationRequest
         * @instance
         */
        FilerJumpLocationRequest.prototype.name = "";
    
        /**
         * Creates a new FilerJumpLocationRequest instance using the specified properties.
         * @function create
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {IFilerJumpLocationRequest=} [properties] Properties to set
         * @returns {FilerJumpLocationRequest} FilerJumpLocationRequest instance
         */
        FilerJumpLocationRequest.create = function create(properties) {
            return new FilerJumpLocationRequest(properties);
        };
    
        /**
         * Encodes the specified FilerJumpLocationRequest message. Does not implicitly {@link FilerJumpLocationRequest.verify|verify} messages.
         * @function encode
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {IFilerJumpLocationRequest} message FilerJumpLocationRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerJumpLocationRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.location != null && message.hasOwnProperty("location"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.location);
            if (message.name != null && message.hasOwnProperty("name"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.name);
            return writer;
        };
    
        /**
         * Encodes the specified FilerJumpLocationRequest message, length delimited. Does not implicitly {@link FilerJumpLocationRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {IFilerJumpLocationRequest} message FilerJumpLocationRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerJumpLocationRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerJumpLocationRequest message from the specified reader or buffer.
         * @function decode
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerJumpLocationRequest} FilerJumpLocationRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerJumpLocationRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerJumpLocationRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.location = reader.string();
                    break;
                case 2:
                    message.name = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerJumpLocationRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerJumpLocationRequest} FilerJumpLocationRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerJumpLocationRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerJumpLocationRequest message.
         * @function verify
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerJumpLocationRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.location != null && message.hasOwnProperty("location"))
                if (!$util.isString(message.location))
                    return "location: string expected";
            if (message.name != null && message.hasOwnProperty("name"))
                if (!$util.isString(message.name))
                    return "name: string expected";
            return null;
        };
    
        /**
         * Creates a FilerJumpLocationRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerJumpLocationRequest} FilerJumpLocationRequest
         */
        FilerJumpLocationRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerJumpLocationRequest)
                return object;
            var message = new $root.FilerJumpLocationRequest();
            if (object.location != null)
                message.location = String(object.location);
            if (object.name != null)
                message.name = String(object.name);
            return message;
        };
    
        /**
         * Creates a plain object from a FilerJumpLocationRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerJumpLocationRequest
         * @static
         * @param {FilerJumpLocationRequest} message FilerJumpLocationRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerJumpLocationRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.location = "";
                object.name = "";
            }
            if (message.location != null && message.hasOwnProperty("location"))
                object.location = message.location;
            if (message.name != null && message.hasOwnProperty("name"))
                object.name = message.name;
            return object;
        };
    
        /**
         * Converts this FilerJumpLocationRequest to JSON.
         * @function toJSON
         * @memberof FilerJumpLocationRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerJumpLocationRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerJumpLocationRequest;
    })();
    
    $root.FilerJumpLocationResponse = (function() {
    
        /**
         * Properties of a FilerJumpLocationResponse.
         * @exports IFilerJumpLocationResponse
         * @interface IFilerJumpLocationResponse
         * @property {IFiler|null} [filer] FilerJumpLocationResponse filer
         */
    
        /**
         * Constructs a new FilerJumpLocationResponse.
         * @exports FilerJumpLocationResponse
         * @classdesc Represents a FilerJumpLocationResponse.
         * @implements IFilerJumpLocationResponse
         * @constructor
         * @param {IFilerJumpLocationResponse=} [properties] Properties to set
         */
        function FilerJumpLocationResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * FilerJumpLocationResponse filer.
         * @member {IFiler|null|undefined} filer
         * @memberof FilerJumpLocationResponse
         * @instance
         */
        FilerJumpLocationResponse.prototype.filer = null;
    
        /**
         * Creates a new FilerJumpLocationResponse instance using the specified properties.
         * @function create
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {IFilerJumpLocationResponse=} [properties] Properties to set
         * @returns {FilerJumpLocationResponse} FilerJumpLocationResponse instance
         */
        FilerJumpLocationResponse.create = function create(properties) {
            return new FilerJumpLocationResponse(properties);
        };
    
        /**
         * Encodes the specified FilerJumpLocationResponse message. Does not implicitly {@link FilerJumpLocationResponse.verify|verify} messages.
         * @function encode
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {IFilerJumpLocationResponse} message FilerJumpLocationResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerJumpLocationResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.filer != null && message.hasOwnProperty("filer"))
                $root.Filer.encode(message.filer, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified FilerJumpLocationResponse message, length delimited. Does not implicitly {@link FilerJumpLocationResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {IFilerJumpLocationResponse} message FilerJumpLocationResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        FilerJumpLocationResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a FilerJumpLocationResponse message from the specified reader or buffer.
         * @function decode
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {FilerJumpLocationResponse} FilerJumpLocationResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerJumpLocationResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.FilerJumpLocationResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.filer = $root.Filer.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a FilerJumpLocationResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {FilerJumpLocationResponse} FilerJumpLocationResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        FilerJumpLocationResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a FilerJumpLocationResponse message.
         * @function verify
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        FilerJumpLocationResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.filer != null && message.hasOwnProperty("filer")) {
                var error = $root.Filer.verify(message.filer);
                if (error)
                    return "filer." + error;
            }
            return null;
        };
    
        /**
         * Creates a FilerJumpLocationResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {FilerJumpLocationResponse} FilerJumpLocationResponse
         */
        FilerJumpLocationResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.FilerJumpLocationResponse)
                return object;
            var message = new $root.FilerJumpLocationResponse();
            if (object.filer != null) {
                if (typeof object.filer !== "object")
                    throw TypeError(".FilerJumpLocationResponse.filer: object expected");
                message.filer = $root.Filer.fromObject(object.filer);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a FilerJumpLocationResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof FilerJumpLocationResponse
         * @static
         * @param {FilerJumpLocationResponse} message FilerJumpLocationResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        FilerJumpLocationResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.filer = null;
            if (message.filer != null && message.hasOwnProperty("filer"))
                object.filer = $root.Filer.toObject(message.filer, options);
            return object;
        };
    
        /**
         * Converts this FilerJumpLocationResponse to JSON.
         * @function toJSON
         * @memberof FilerJumpLocationResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        FilerJumpLocationResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return FilerJumpLocationResponse;
    })();
    
    /**
     * SortType enum.
     * @exports SortType
     * @enum {string}
     * @property {number} Name=0 Name value
     * @property {number} Size=1 Size value
     * @property {number} Date=2 Date value
     */
    $root.SortType = (function() {
        var valuesById = {}, values = Object.create(valuesById);
        values[valuesById[0] = "Name"] = 0;
        values[valuesById[1] = "Size"] = 1;
        values[valuesById[2] = "Date"] = 2;
        return values;
    })();

    return $root;
});
