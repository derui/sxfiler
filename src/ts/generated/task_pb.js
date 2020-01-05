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
    
    /**
     * ReplyType enum.
     * @exports ReplyType
     * @enum {string}
     * @property {number} Overwrite=0 Overwrite value
     * @property {number} Rename=1 Rename value
     */
    $root.ReplyType = (function() {
        var valuesById = {}, values = Object.create(valuesById);
        values[valuesById[0] = "Overwrite"] = 0;
        values[valuesById[1] = "Rename"] = 1;
        return values;
    })();
    
    $root.TaskReply = (function() {
    
        /**
         * Properties of a TaskReply.
         * @exports ITaskReply
         * @interface ITaskReply
         * @property {ReplyType|null} [type] TaskReply type
         * @property {boolean|null} [overwrite] TaskReply overwrite
         * @property {TaskReply.IRename|null} [rename] TaskReply rename
         * @property {string|null} [taskId] TaskReply taskId
         */
    
        /**
         * Constructs a new TaskReply.
         * @exports TaskReply
         * @classdesc Represents a TaskReply.
         * @implements ITaskReply
         * @constructor
         * @param {ITaskReply=} [properties] Properties to set
         */
        function TaskReply(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * TaskReply type.
         * @member {ReplyType} type
         * @memberof TaskReply
         * @instance
         */
        TaskReply.prototype.type = 0;
    
        /**
         * TaskReply overwrite.
         * @member {boolean} overwrite
         * @memberof TaskReply
         * @instance
         */
        TaskReply.prototype.overwrite = false;
    
        /**
         * TaskReply rename.
         * @member {TaskReply.IRename|null|undefined} rename
         * @memberof TaskReply
         * @instance
         */
        TaskReply.prototype.rename = null;
    
        /**
         * TaskReply taskId.
         * @member {string} taskId
         * @memberof TaskReply
         * @instance
         */
        TaskReply.prototype.taskId = "";
    
        // OneOf field names bound to virtual getters and setters
        var $oneOfFields;
    
        /**
         * TaskReply reply.
         * @member {"overwrite"|"rename"|undefined} reply
         * @memberof TaskReply
         * @instance
         */
        Object.defineProperty(TaskReply.prototype, "reply", {
            get: $util.oneOfGetter($oneOfFields = ["overwrite", "rename"]),
            set: $util.oneOfSetter($oneOfFields)
        });
    
        /**
         * Creates a new TaskReply instance using the specified properties.
         * @function create
         * @memberof TaskReply
         * @static
         * @param {ITaskReply=} [properties] Properties to set
         * @returns {TaskReply} TaskReply instance
         */
        TaskReply.create = function create(properties) {
            return new TaskReply(properties);
        };
    
        /**
         * Encodes the specified TaskReply message. Does not implicitly {@link TaskReply.verify|verify} messages.
         * @function encode
         * @memberof TaskReply
         * @static
         * @param {ITaskReply} message TaskReply message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReply.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.type != null && message.hasOwnProperty("type"))
                writer.uint32(/* id 1, wireType 0 =*/8).int32(message.type);
            if (message.overwrite != null && message.hasOwnProperty("overwrite"))
                writer.uint32(/* id 2, wireType 0 =*/16).bool(message.overwrite);
            if (message.rename != null && message.hasOwnProperty("rename"))
                $root.TaskReply.Rename.encode(message.rename, writer.uint32(/* id 3, wireType 2 =*/26).fork()).ldelim();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 4, wireType 2 =*/34).string(message.taskId);
            return writer;
        };
    
        /**
         * Encodes the specified TaskReply message, length delimited. Does not implicitly {@link TaskReply.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskReply
         * @static
         * @param {ITaskReply} message TaskReply message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReply.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskReply message from the specified reader or buffer.
         * @function decode
         * @memberof TaskReply
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskReply} TaskReply
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReply.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskReply();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.type = reader.int32();
                    break;
                case 2:
                    message.overwrite = reader.bool();
                    break;
                case 3:
                    message.rename = $root.TaskReply.Rename.decode(reader, reader.uint32());
                    break;
                case 4:
                    message.taskId = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskReply message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskReply
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskReply} TaskReply
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReply.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskReply message.
         * @function verify
         * @memberof TaskReply
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskReply.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            var properties = {};
            if (message.type != null && message.hasOwnProperty("type"))
                switch (message.type) {
                default:
                    return "type: enum value expected";
                case 0:
                case 1:
                    break;
                }
            if (message.overwrite != null && message.hasOwnProperty("overwrite")) {
                properties.reply = 1;
                if (typeof message.overwrite !== "boolean")
                    return "overwrite: boolean expected";
            }
            if (message.rename != null && message.hasOwnProperty("rename")) {
                if (properties.reply === 1)
                    return "reply: multiple values";
                properties.reply = 1;
                {
                    var error = $root.TaskReply.Rename.verify(message.rename);
                    if (error)
                        return "rename." + error;
                }
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            return null;
        };
    
        /**
         * Creates a TaskReply message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskReply
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskReply} TaskReply
         */
        TaskReply.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskReply)
                return object;
            var message = new $root.TaskReply();
            switch (object.type) {
            case "Overwrite":
            case 0:
                message.type = 0;
                break;
            case "Rename":
            case 1:
                message.type = 1;
                break;
            }
            if (object.overwrite != null)
                message.overwrite = Boolean(object.overwrite);
            if (object.rename != null) {
                if (typeof object.rename !== "object")
                    throw TypeError(".TaskReply.rename: object expected");
                message.rename = $root.TaskReply.Rename.fromObject(object.rename);
            }
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            return message;
        };
    
        /**
         * Creates a plain object from a TaskReply message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskReply
         * @static
         * @param {TaskReply} message TaskReply
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskReply.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.type = options.enums === String ? "Overwrite" : 0;
                object.taskId = "";
            }
            if (message.type != null && message.hasOwnProperty("type"))
                object.type = options.enums === String ? $root.ReplyType[message.type] : message.type;
            if (message.overwrite != null && message.hasOwnProperty("overwrite")) {
                object.overwrite = message.overwrite;
                if (options.oneofs)
                    object.reply = "overwrite";
            }
            if (message.rename != null && message.hasOwnProperty("rename")) {
                object.rename = $root.TaskReply.Rename.toObject(message.rename, options);
                if (options.oneofs)
                    object.reply = "rename";
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            return object;
        };
    
        /**
         * Converts this TaskReply to JSON.
         * @function toJSON
         * @memberof TaskReply
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskReply.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        TaskReply.Rename = (function() {
    
            /**
             * Properties of a Rename.
             * @memberof TaskReply
             * @interface IRename
             * @property {string|null} [newName] Rename newName
             */
    
            /**
             * Constructs a new Rename.
             * @memberof TaskReply
             * @classdesc Represents a Rename.
             * @implements IRename
             * @constructor
             * @param {TaskReply.IRename=} [properties] Properties to set
             */
            function Rename(properties) {
                if (properties)
                    for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                        if (properties[keys[i]] != null)
                            this[keys[i]] = properties[keys[i]];
            }
    
            /**
             * Rename newName.
             * @member {string} newName
             * @memberof TaskReply.Rename
             * @instance
             */
            Rename.prototype.newName = "";
    
            /**
             * Creates a new Rename instance using the specified properties.
             * @function create
             * @memberof TaskReply.Rename
             * @static
             * @param {TaskReply.IRename=} [properties] Properties to set
             * @returns {TaskReply.Rename} Rename instance
             */
            Rename.create = function create(properties) {
                return new Rename(properties);
            };
    
            /**
             * Encodes the specified Rename message. Does not implicitly {@link TaskReply.Rename.verify|verify} messages.
             * @function encode
             * @memberof TaskReply.Rename
             * @static
             * @param {TaskReply.IRename} message Rename message or plain object to encode
             * @param {$protobuf.Writer} [writer] Writer to encode to
             * @returns {$protobuf.Writer} Writer
             */
            Rename.encode = function encode(message, writer) {
                if (!writer)
                    writer = $Writer.create();
                if (message.newName != null && message.hasOwnProperty("newName"))
                    writer.uint32(/* id 1, wireType 2 =*/10).string(message.newName);
                return writer;
            };
    
            /**
             * Encodes the specified Rename message, length delimited. Does not implicitly {@link TaskReply.Rename.verify|verify} messages.
             * @function encodeDelimited
             * @memberof TaskReply.Rename
             * @static
             * @param {TaskReply.IRename} message Rename message or plain object to encode
             * @param {$protobuf.Writer} [writer] Writer to encode to
             * @returns {$protobuf.Writer} Writer
             */
            Rename.encodeDelimited = function encodeDelimited(message, writer) {
                return this.encode(message, writer).ldelim();
            };
    
            /**
             * Decodes a Rename message from the specified reader or buffer.
             * @function decode
             * @memberof TaskReply.Rename
             * @static
             * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
             * @param {number} [length] Message length if known beforehand
             * @returns {TaskReply.Rename} Rename
             * @throws {Error} If the payload is not a reader or valid buffer
             * @throws {$protobuf.util.ProtocolError} If required fields are missing
             */
            Rename.decode = function decode(reader, length) {
                if (!(reader instanceof $Reader))
                    reader = $Reader.create(reader);
                var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskReply.Rename();
                while (reader.pos < end) {
                    var tag = reader.uint32();
                    switch (tag >>> 3) {
                    case 1:
                        message.newName = reader.string();
                        break;
                    default:
                        reader.skipType(tag & 7);
                        break;
                    }
                }
                return message;
            };
    
            /**
             * Decodes a Rename message from the specified reader or buffer, length delimited.
             * @function decodeDelimited
             * @memberof TaskReply.Rename
             * @static
             * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
             * @returns {TaskReply.Rename} Rename
             * @throws {Error} If the payload is not a reader or valid buffer
             * @throws {$protobuf.util.ProtocolError} If required fields are missing
             */
            Rename.decodeDelimited = function decodeDelimited(reader) {
                if (!(reader instanceof $Reader))
                    reader = new $Reader(reader);
                return this.decode(reader, reader.uint32());
            };
    
            /**
             * Verifies a Rename message.
             * @function verify
             * @memberof TaskReply.Rename
             * @static
             * @param {Object.<string,*>} message Plain object to verify
             * @returns {string|null} `null` if valid, otherwise the reason why it is not
             */
            Rename.verify = function verify(message) {
                if (typeof message !== "object" || message === null)
                    return "object expected";
                if (message.newName != null && message.hasOwnProperty("newName"))
                    if (!$util.isString(message.newName))
                        return "newName: string expected";
                return null;
            };
    
            /**
             * Creates a Rename message from a plain object. Also converts values to their respective internal types.
             * @function fromObject
             * @memberof TaskReply.Rename
             * @static
             * @param {Object.<string,*>} object Plain object
             * @returns {TaskReply.Rename} Rename
             */
            Rename.fromObject = function fromObject(object) {
                if (object instanceof $root.TaskReply.Rename)
                    return object;
                var message = new $root.TaskReply.Rename();
                if (object.newName != null)
                    message.newName = String(object.newName);
                return message;
            };
    
            /**
             * Creates a plain object from a Rename message. Also converts values to other types if specified.
             * @function toObject
             * @memberof TaskReply.Rename
             * @static
             * @param {TaskReply.Rename} message Rename
             * @param {$protobuf.IConversionOptions} [options] Conversion options
             * @returns {Object.<string,*>} Plain object
             */
            Rename.toObject = function toObject(message, options) {
                if (!options)
                    options = {};
                var object = {};
                if (options.defaults)
                    object.newName = "";
                if (message.newName != null && message.hasOwnProperty("newName"))
                    object.newName = message.newName;
                return object;
            };
    
            /**
             * Converts this Rename to JSON.
             * @function toJSON
             * @memberof TaskReply.Rename
             * @instance
             * @returns {Object.<string,*>} JSON object
             */
            Rename.prototype.toJSON = function toJSON() {
                return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
            };
    
            return Rename;
        })();
    
        return TaskReply;
    })();
    
    $root.TaskSuggestion = (function() {
    
        /**
         * Properties of a TaskSuggestion.
         * @exports ITaskSuggestion
         * @interface ITaskSuggestion
         * @property {Array.<ReplyType>|null} [suggestions] TaskSuggestion suggestions
         * @property {string|null} [itemName] TaskSuggestion itemName
         * @property {string|null} [taskId] TaskSuggestion taskId
         */
    
        /**
         * Constructs a new TaskSuggestion.
         * @exports TaskSuggestion
         * @classdesc Represents a TaskSuggestion.
         * @implements ITaskSuggestion
         * @constructor
         * @param {ITaskSuggestion=} [properties] Properties to set
         */
        function TaskSuggestion(properties) {
            this.suggestions = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * TaskSuggestion suggestions.
         * @member {Array.<ReplyType>} suggestions
         * @memberof TaskSuggestion
         * @instance
         */
        TaskSuggestion.prototype.suggestions = $util.emptyArray;
    
        /**
         * TaskSuggestion itemName.
         * @member {string} itemName
         * @memberof TaskSuggestion
         * @instance
         */
        TaskSuggestion.prototype.itemName = "";
    
        /**
         * TaskSuggestion taskId.
         * @member {string} taskId
         * @memberof TaskSuggestion
         * @instance
         */
        TaskSuggestion.prototype.taskId = "";
    
        /**
         * Creates a new TaskSuggestion instance using the specified properties.
         * @function create
         * @memberof TaskSuggestion
         * @static
         * @param {ITaskSuggestion=} [properties] Properties to set
         * @returns {TaskSuggestion} TaskSuggestion instance
         */
        TaskSuggestion.create = function create(properties) {
            return new TaskSuggestion(properties);
        };
    
        /**
         * Encodes the specified TaskSuggestion message. Does not implicitly {@link TaskSuggestion.verify|verify} messages.
         * @function encode
         * @memberof TaskSuggestion
         * @static
         * @param {ITaskSuggestion} message TaskSuggestion message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskSuggestion.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.suggestions != null && message.suggestions.length) {
                writer.uint32(/* id 1, wireType 2 =*/10).fork();
                for (var i = 0; i < message.suggestions.length; ++i)
                    writer.int32(message.suggestions[i]);
                writer.ldelim();
            }
            if (message.itemName != null && message.hasOwnProperty("itemName"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.itemName);
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 3, wireType 2 =*/26).string(message.taskId);
            return writer;
        };
    
        /**
         * Encodes the specified TaskSuggestion message, length delimited. Does not implicitly {@link TaskSuggestion.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskSuggestion
         * @static
         * @param {ITaskSuggestion} message TaskSuggestion message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskSuggestion.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskSuggestion message from the specified reader or buffer.
         * @function decode
         * @memberof TaskSuggestion
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskSuggestion} TaskSuggestion
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskSuggestion.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskSuggestion();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    if (!(message.suggestions && message.suggestions.length))
                        message.suggestions = [];
                    if ((tag & 7) === 2) {
                        var end2 = reader.uint32() + reader.pos;
                        while (reader.pos < end2)
                            message.suggestions.push(reader.int32());
                    } else
                        message.suggestions.push(reader.int32());
                    break;
                case 2:
                    message.itemName = reader.string();
                    break;
                case 3:
                    message.taskId = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskSuggestion message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskSuggestion
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskSuggestion} TaskSuggestion
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskSuggestion.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskSuggestion message.
         * @function verify
         * @memberof TaskSuggestion
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskSuggestion.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.suggestions != null && message.hasOwnProperty("suggestions")) {
                if (!Array.isArray(message.suggestions))
                    return "suggestions: array expected";
                for (var i = 0; i < message.suggestions.length; ++i)
                    switch (message.suggestions[i]) {
                    default:
                        return "suggestions: enum value[] expected";
                    case 0:
                    case 1:
                        break;
                    }
            }
            if (message.itemName != null && message.hasOwnProperty("itemName"))
                if (!$util.isString(message.itemName))
                    return "itemName: string expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            return null;
        };
    
        /**
         * Creates a TaskSuggestion message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskSuggestion
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskSuggestion} TaskSuggestion
         */
        TaskSuggestion.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskSuggestion)
                return object;
            var message = new $root.TaskSuggestion();
            if (object.suggestions) {
                if (!Array.isArray(object.suggestions))
                    throw TypeError(".TaskSuggestion.suggestions: array expected");
                message.suggestions = [];
                for (var i = 0; i < object.suggestions.length; ++i)
                    switch (object.suggestions[i]) {
                    default:
                    case "Overwrite":
                    case 0:
                        message.suggestions[i] = 0;
                        break;
                    case "Rename":
                    case 1:
                        message.suggestions[i] = 1;
                        break;
                    }
            }
            if (object.itemName != null)
                message.itemName = String(object.itemName);
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            return message;
        };
    
        /**
         * Creates a plain object from a TaskSuggestion message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskSuggestion
         * @static
         * @param {TaskSuggestion} message TaskSuggestion
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskSuggestion.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.suggestions = [];
            if (options.defaults) {
                object.itemName = "";
                object.taskId = "";
            }
            if (message.suggestions && message.suggestions.length) {
                object.suggestions = [];
                for (var j = 0; j < message.suggestions.length; ++j)
                    object.suggestions[j] = options.enums === String ? $root.ReplyType[message.suggestions[j]] : message.suggestions[j];
            }
            if (message.itemName != null && message.hasOwnProperty("itemName"))
                object.itemName = message.itemName;
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            return object;
        };
    
        /**
         * Converts this TaskSuggestion to JSON.
         * @function toJSON
         * @memberof TaskSuggestion
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskSuggestion.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskSuggestion;
    })();
    
    $root.TaskSendReplyRequest = (function() {
    
        /**
         * Properties of a TaskSendReplyRequest.
         * @exports ITaskSendReplyRequest
         * @interface ITaskSendReplyRequest
         * @property {ITaskReply|null} [reply] TaskSendReplyRequest reply
         */
    
        /**
         * Constructs a new TaskSendReplyRequest.
         * @exports TaskSendReplyRequest
         * @classdesc Represents a TaskSendReplyRequest.
         * @implements ITaskSendReplyRequest
         * @constructor
         * @param {ITaskSendReplyRequest=} [properties] Properties to set
         */
        function TaskSendReplyRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * TaskSendReplyRequest reply.
         * @member {ITaskReply|null|undefined} reply
         * @memberof TaskSendReplyRequest
         * @instance
         */
        TaskSendReplyRequest.prototype.reply = null;
    
        /**
         * Creates a new TaskSendReplyRequest instance using the specified properties.
         * @function create
         * @memberof TaskSendReplyRequest
         * @static
         * @param {ITaskSendReplyRequest=} [properties] Properties to set
         * @returns {TaskSendReplyRequest} TaskSendReplyRequest instance
         */
        TaskSendReplyRequest.create = function create(properties) {
            return new TaskSendReplyRequest(properties);
        };
    
        /**
         * Encodes the specified TaskSendReplyRequest message. Does not implicitly {@link TaskSendReplyRequest.verify|verify} messages.
         * @function encode
         * @memberof TaskSendReplyRequest
         * @static
         * @param {ITaskSendReplyRequest} message TaskSendReplyRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskSendReplyRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.reply != null && message.hasOwnProperty("reply"))
                $root.TaskReply.encode(message.reply, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified TaskSendReplyRequest message, length delimited. Does not implicitly {@link TaskSendReplyRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskSendReplyRequest
         * @static
         * @param {ITaskSendReplyRequest} message TaskSendReplyRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskSendReplyRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskSendReplyRequest message from the specified reader or buffer.
         * @function decode
         * @memberof TaskSendReplyRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskSendReplyRequest} TaskSendReplyRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskSendReplyRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskSendReplyRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.reply = $root.TaskReply.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskSendReplyRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskSendReplyRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskSendReplyRequest} TaskSendReplyRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskSendReplyRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskSendReplyRequest message.
         * @function verify
         * @memberof TaskSendReplyRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskSendReplyRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.reply != null && message.hasOwnProperty("reply")) {
                var error = $root.TaskReply.verify(message.reply);
                if (error)
                    return "reply." + error;
            }
            return null;
        };
    
        /**
         * Creates a TaskSendReplyRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskSendReplyRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskSendReplyRequest} TaskSendReplyRequest
         */
        TaskSendReplyRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskSendReplyRequest)
                return object;
            var message = new $root.TaskSendReplyRequest();
            if (object.reply != null) {
                if (typeof object.reply !== "object")
                    throw TypeError(".TaskSendReplyRequest.reply: object expected");
                message.reply = $root.TaskReply.fromObject(object.reply);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a TaskSendReplyRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskSendReplyRequest
         * @static
         * @param {TaskSendReplyRequest} message TaskSendReplyRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskSendReplyRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.reply = null;
            if (message.reply != null && message.hasOwnProperty("reply"))
                object.reply = $root.TaskReply.toObject(message.reply, options);
            return object;
        };
    
        /**
         * Converts this TaskSendReplyRequest to JSON.
         * @function toJSON
         * @memberof TaskSendReplyRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskSendReplyRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskSendReplyRequest;
    })();
    
    $root.TaskSendReplyResponse = (function() {
    
        /**
         * Properties of a TaskSendReplyResponse.
         * @exports ITaskSendReplyResponse
         * @interface ITaskSendReplyResponse
         */
    
        /**
         * Constructs a new TaskSendReplyResponse.
         * @exports TaskSendReplyResponse
         * @classdesc Represents a TaskSendReplyResponse.
         * @implements ITaskSendReplyResponse
         * @constructor
         * @param {ITaskSendReplyResponse=} [properties] Properties to set
         */
        function TaskSendReplyResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new TaskSendReplyResponse instance using the specified properties.
         * @function create
         * @memberof TaskSendReplyResponse
         * @static
         * @param {ITaskSendReplyResponse=} [properties] Properties to set
         * @returns {TaskSendReplyResponse} TaskSendReplyResponse instance
         */
        TaskSendReplyResponse.create = function create(properties) {
            return new TaskSendReplyResponse(properties);
        };
    
        /**
         * Encodes the specified TaskSendReplyResponse message. Does not implicitly {@link TaskSendReplyResponse.verify|verify} messages.
         * @function encode
         * @memberof TaskSendReplyResponse
         * @static
         * @param {ITaskSendReplyResponse} message TaskSendReplyResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskSendReplyResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified TaskSendReplyResponse message, length delimited. Does not implicitly {@link TaskSendReplyResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskSendReplyResponse
         * @static
         * @param {ITaskSendReplyResponse} message TaskSendReplyResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskSendReplyResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskSendReplyResponse message from the specified reader or buffer.
         * @function decode
         * @memberof TaskSendReplyResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskSendReplyResponse} TaskSendReplyResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskSendReplyResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskSendReplyResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskSendReplyResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskSendReplyResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskSendReplyResponse} TaskSendReplyResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskSendReplyResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskSendReplyResponse message.
         * @function verify
         * @memberof TaskSendReplyResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskSendReplyResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a TaskSendReplyResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskSendReplyResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskSendReplyResponse} TaskSendReplyResponse
         */
        TaskSendReplyResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskSendReplyResponse)
                return object;
            return new $root.TaskSendReplyResponse();
        };
    
        /**
         * Creates a plain object from a TaskSendReplyResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskSendReplyResponse
         * @static
         * @param {TaskSendReplyResponse} message TaskSendReplyResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskSendReplyResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this TaskSendReplyResponse to JSON.
         * @function toJSON
         * @memberof TaskSendReplyResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskSendReplyResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskSendReplyResponse;
    })();
    
    $root.TaskCancelRequest = (function() {
    
        /**
         * Properties of a TaskCancelRequest.
         * @exports ITaskCancelRequest
         * @interface ITaskCancelRequest
         * @property {string|null} [taskId] TaskCancelRequest taskId
         */
    
        /**
         * Constructs a new TaskCancelRequest.
         * @exports TaskCancelRequest
         * @classdesc Represents a TaskCancelRequest.
         * @implements ITaskCancelRequest
         * @constructor
         * @param {ITaskCancelRequest=} [properties] Properties to set
         */
        function TaskCancelRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * TaskCancelRequest taskId.
         * @member {string} taskId
         * @memberof TaskCancelRequest
         * @instance
         */
        TaskCancelRequest.prototype.taskId = "";
    
        /**
         * Creates a new TaskCancelRequest instance using the specified properties.
         * @function create
         * @memberof TaskCancelRequest
         * @static
         * @param {ITaskCancelRequest=} [properties] Properties to set
         * @returns {TaskCancelRequest} TaskCancelRequest instance
         */
        TaskCancelRequest.create = function create(properties) {
            return new TaskCancelRequest(properties);
        };
    
        /**
         * Encodes the specified TaskCancelRequest message. Does not implicitly {@link TaskCancelRequest.verify|verify} messages.
         * @function encode
         * @memberof TaskCancelRequest
         * @static
         * @param {ITaskCancelRequest} message TaskCancelRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskCancelRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.taskId);
            return writer;
        };
    
        /**
         * Encodes the specified TaskCancelRequest message, length delimited. Does not implicitly {@link TaskCancelRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskCancelRequest
         * @static
         * @param {ITaskCancelRequest} message TaskCancelRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskCancelRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskCancelRequest message from the specified reader or buffer.
         * @function decode
         * @memberof TaskCancelRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskCancelRequest} TaskCancelRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskCancelRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskCancelRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.taskId = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskCancelRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskCancelRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskCancelRequest} TaskCancelRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskCancelRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskCancelRequest message.
         * @function verify
         * @memberof TaskCancelRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskCancelRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            return null;
        };
    
        /**
         * Creates a TaskCancelRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskCancelRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskCancelRequest} TaskCancelRequest
         */
        TaskCancelRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskCancelRequest)
                return object;
            var message = new $root.TaskCancelRequest();
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            return message;
        };
    
        /**
         * Creates a plain object from a TaskCancelRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskCancelRequest
         * @static
         * @param {TaskCancelRequest} message TaskCancelRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskCancelRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.taskId = "";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            return object;
        };
    
        /**
         * Converts this TaskCancelRequest to JSON.
         * @function toJSON
         * @memberof TaskCancelRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskCancelRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskCancelRequest;
    })();
    
    $root.TaskCancelResponse = (function() {
    
        /**
         * Properties of a TaskCancelResponse.
         * @exports ITaskCancelResponse
         * @interface ITaskCancelResponse
         */
    
        /**
         * Constructs a new TaskCancelResponse.
         * @exports TaskCancelResponse
         * @classdesc Represents a TaskCancelResponse.
         * @implements ITaskCancelResponse
         * @constructor
         * @param {ITaskCancelResponse=} [properties] Properties to set
         */
        function TaskCancelResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new TaskCancelResponse instance using the specified properties.
         * @function create
         * @memberof TaskCancelResponse
         * @static
         * @param {ITaskCancelResponse=} [properties] Properties to set
         * @returns {TaskCancelResponse} TaskCancelResponse instance
         */
        TaskCancelResponse.create = function create(properties) {
            return new TaskCancelResponse(properties);
        };
    
        /**
         * Encodes the specified TaskCancelResponse message. Does not implicitly {@link TaskCancelResponse.verify|verify} messages.
         * @function encode
         * @memberof TaskCancelResponse
         * @static
         * @param {ITaskCancelResponse} message TaskCancelResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskCancelResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified TaskCancelResponse message, length delimited. Does not implicitly {@link TaskCancelResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskCancelResponse
         * @static
         * @param {ITaskCancelResponse} message TaskCancelResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskCancelResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskCancelResponse message from the specified reader or buffer.
         * @function decode
         * @memberof TaskCancelResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskCancelResponse} TaskCancelResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskCancelResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskCancelResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskCancelResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskCancelResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskCancelResponse} TaskCancelResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskCancelResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskCancelResponse message.
         * @function verify
         * @memberof TaskCancelResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskCancelResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a TaskCancelResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskCancelResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskCancelResponse} TaskCancelResponse
         */
        TaskCancelResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskCancelResponse)
                return object;
            return new $root.TaskCancelResponse();
        };
    
        /**
         * Creates a plain object from a TaskCancelResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskCancelResponse
         * @static
         * @param {TaskCancelResponse} message TaskCancelResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskCancelResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this TaskCancelResponse to JSON.
         * @function toJSON
         * @memberof TaskCancelResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskCancelResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskCancelResponse;
    })();

    return $root;
});
