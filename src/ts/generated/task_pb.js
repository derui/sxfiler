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
    
    $root.TaskReplyToOverwriteRequest = (function() {
    
        /**
         * Properties of a TaskReplyToOverwriteRequest.
         * @exports ITaskReplyToOverwriteRequest
         * @interface ITaskReplyToOverwriteRequest
         * @property {string|null} [taskId] TaskReplyToOverwriteRequest taskId
         * @property {boolean|null} [overwrite] TaskReplyToOverwriteRequest overwrite
         */
    
        /**
         * Constructs a new TaskReplyToOverwriteRequest.
         * @exports TaskReplyToOverwriteRequest
         * @classdesc Represents a TaskReplyToOverwriteRequest.
         * @implements ITaskReplyToOverwriteRequest
         * @constructor
         * @param {ITaskReplyToOverwriteRequest=} [properties] Properties to set
         */
        function TaskReplyToOverwriteRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * TaskReplyToOverwriteRequest taskId.
         * @member {string} taskId
         * @memberof TaskReplyToOverwriteRequest
         * @instance
         */
        TaskReplyToOverwriteRequest.prototype.taskId = "";
    
        /**
         * TaskReplyToOverwriteRequest overwrite.
         * @member {boolean} overwrite
         * @memberof TaskReplyToOverwriteRequest
         * @instance
         */
        TaskReplyToOverwriteRequest.prototype.overwrite = false;
    
        /**
         * Creates a new TaskReplyToOverwriteRequest instance using the specified properties.
         * @function create
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {ITaskReplyToOverwriteRequest=} [properties] Properties to set
         * @returns {TaskReplyToOverwriteRequest} TaskReplyToOverwriteRequest instance
         */
        TaskReplyToOverwriteRequest.create = function create(properties) {
            return new TaskReplyToOverwriteRequest(properties);
        };
    
        /**
         * Encodes the specified TaskReplyToOverwriteRequest message. Does not implicitly {@link TaskReplyToOverwriteRequest.verify|verify} messages.
         * @function encode
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {ITaskReplyToOverwriteRequest} message TaskReplyToOverwriteRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToOverwriteRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.taskId);
            if (message.overwrite != null && message.hasOwnProperty("overwrite"))
                writer.uint32(/* id 2, wireType 0 =*/16).bool(message.overwrite);
            return writer;
        };
    
        /**
         * Encodes the specified TaskReplyToOverwriteRequest message, length delimited. Does not implicitly {@link TaskReplyToOverwriteRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {ITaskReplyToOverwriteRequest} message TaskReplyToOverwriteRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToOverwriteRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskReplyToOverwriteRequest message from the specified reader or buffer.
         * @function decode
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskReplyToOverwriteRequest} TaskReplyToOverwriteRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToOverwriteRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskReplyToOverwriteRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.taskId = reader.string();
                    break;
                case 2:
                    message.overwrite = reader.bool();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a TaskReplyToOverwriteRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskReplyToOverwriteRequest} TaskReplyToOverwriteRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToOverwriteRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskReplyToOverwriteRequest message.
         * @function verify
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskReplyToOverwriteRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            if (message.overwrite != null && message.hasOwnProperty("overwrite"))
                if (typeof message.overwrite !== "boolean")
                    return "overwrite: boolean expected";
            return null;
        };
    
        /**
         * Creates a TaskReplyToOverwriteRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskReplyToOverwriteRequest} TaskReplyToOverwriteRequest
         */
        TaskReplyToOverwriteRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskReplyToOverwriteRequest)
                return object;
            var message = new $root.TaskReplyToOverwriteRequest();
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            if (object.overwrite != null)
                message.overwrite = Boolean(object.overwrite);
            return message;
        };
    
        /**
         * Creates a plain object from a TaskReplyToOverwriteRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskReplyToOverwriteRequest
         * @static
         * @param {TaskReplyToOverwriteRequest} message TaskReplyToOverwriteRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskReplyToOverwriteRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.taskId = "";
                object.overwrite = false;
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            if (message.overwrite != null && message.hasOwnProperty("overwrite"))
                object.overwrite = message.overwrite;
            return object;
        };
    
        /**
         * Converts this TaskReplyToOverwriteRequest to JSON.
         * @function toJSON
         * @memberof TaskReplyToOverwriteRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskReplyToOverwriteRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskReplyToOverwriteRequest;
    })();
    
    $root.TaskReplyToOverwriteResponse = (function() {
    
        /**
         * Properties of a TaskReplyToOverwriteResponse.
         * @exports ITaskReplyToOverwriteResponse
         * @interface ITaskReplyToOverwriteResponse
         */
    
        /**
         * Constructs a new TaskReplyToOverwriteResponse.
         * @exports TaskReplyToOverwriteResponse
         * @classdesc Represents a TaskReplyToOverwriteResponse.
         * @implements ITaskReplyToOverwriteResponse
         * @constructor
         * @param {ITaskReplyToOverwriteResponse=} [properties] Properties to set
         */
        function TaskReplyToOverwriteResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new TaskReplyToOverwriteResponse instance using the specified properties.
         * @function create
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {ITaskReplyToOverwriteResponse=} [properties] Properties to set
         * @returns {TaskReplyToOverwriteResponse} TaskReplyToOverwriteResponse instance
         */
        TaskReplyToOverwriteResponse.create = function create(properties) {
            return new TaskReplyToOverwriteResponse(properties);
        };
    
        /**
         * Encodes the specified TaskReplyToOverwriteResponse message. Does not implicitly {@link TaskReplyToOverwriteResponse.verify|verify} messages.
         * @function encode
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {ITaskReplyToOverwriteResponse} message TaskReplyToOverwriteResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToOverwriteResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified TaskReplyToOverwriteResponse message, length delimited. Does not implicitly {@link TaskReplyToOverwriteResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {ITaskReplyToOverwriteResponse} message TaskReplyToOverwriteResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToOverwriteResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskReplyToOverwriteResponse message from the specified reader or buffer.
         * @function decode
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskReplyToOverwriteResponse} TaskReplyToOverwriteResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToOverwriteResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskReplyToOverwriteResponse();
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
         * Decodes a TaskReplyToOverwriteResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskReplyToOverwriteResponse} TaskReplyToOverwriteResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToOverwriteResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskReplyToOverwriteResponse message.
         * @function verify
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskReplyToOverwriteResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a TaskReplyToOverwriteResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskReplyToOverwriteResponse} TaskReplyToOverwriteResponse
         */
        TaskReplyToOverwriteResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskReplyToOverwriteResponse)
                return object;
            return new $root.TaskReplyToOverwriteResponse();
        };
    
        /**
         * Creates a plain object from a TaskReplyToOverwriteResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskReplyToOverwriteResponse
         * @static
         * @param {TaskReplyToOverwriteResponse} message TaskReplyToOverwriteResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskReplyToOverwriteResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this TaskReplyToOverwriteResponse to JSON.
         * @function toJSON
         * @memberof TaskReplyToOverwriteResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskReplyToOverwriteResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskReplyToOverwriteResponse;
    })();
    
    $root.TaskReplyToRenameRequest = (function() {
    
        /**
         * Properties of a TaskReplyToRenameRequest.
         * @exports ITaskReplyToRenameRequest
         * @interface ITaskReplyToRenameRequest
         * @property {string|null} [taskId] TaskReplyToRenameRequest taskId
         * @property {string|null} [newName] TaskReplyToRenameRequest newName
         */
    
        /**
         * Constructs a new TaskReplyToRenameRequest.
         * @exports TaskReplyToRenameRequest
         * @classdesc Represents a TaskReplyToRenameRequest.
         * @implements ITaskReplyToRenameRequest
         * @constructor
         * @param {ITaskReplyToRenameRequest=} [properties] Properties to set
         */
        function TaskReplyToRenameRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * TaskReplyToRenameRequest taskId.
         * @member {string} taskId
         * @memberof TaskReplyToRenameRequest
         * @instance
         */
        TaskReplyToRenameRequest.prototype.taskId = "";
    
        /**
         * TaskReplyToRenameRequest newName.
         * @member {string} newName
         * @memberof TaskReplyToRenameRequest
         * @instance
         */
        TaskReplyToRenameRequest.prototype.newName = "";
    
        /**
         * Creates a new TaskReplyToRenameRequest instance using the specified properties.
         * @function create
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {ITaskReplyToRenameRequest=} [properties] Properties to set
         * @returns {TaskReplyToRenameRequest} TaskReplyToRenameRequest instance
         */
        TaskReplyToRenameRequest.create = function create(properties) {
            return new TaskReplyToRenameRequest(properties);
        };
    
        /**
         * Encodes the specified TaskReplyToRenameRequest message. Does not implicitly {@link TaskReplyToRenameRequest.verify|verify} messages.
         * @function encode
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {ITaskReplyToRenameRequest} message TaskReplyToRenameRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToRenameRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.taskId);
            if (message.newName != null && message.hasOwnProperty("newName"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.newName);
            return writer;
        };
    
        /**
         * Encodes the specified TaskReplyToRenameRequest message, length delimited. Does not implicitly {@link TaskReplyToRenameRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {ITaskReplyToRenameRequest} message TaskReplyToRenameRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToRenameRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskReplyToRenameRequest message from the specified reader or buffer.
         * @function decode
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskReplyToRenameRequest} TaskReplyToRenameRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToRenameRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskReplyToRenameRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.taskId = reader.string();
                    break;
                case 2:
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
         * Decodes a TaskReplyToRenameRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskReplyToRenameRequest} TaskReplyToRenameRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToRenameRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskReplyToRenameRequest message.
         * @function verify
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskReplyToRenameRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                if (!$util.isString(message.taskId))
                    return "taskId: string expected";
            if (message.newName != null && message.hasOwnProperty("newName"))
                if (!$util.isString(message.newName))
                    return "newName: string expected";
            return null;
        };
    
        /**
         * Creates a TaskReplyToRenameRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskReplyToRenameRequest} TaskReplyToRenameRequest
         */
        TaskReplyToRenameRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskReplyToRenameRequest)
                return object;
            var message = new $root.TaskReplyToRenameRequest();
            if (object.taskId != null)
                message.taskId = String(object.taskId);
            if (object.newName != null)
                message.newName = String(object.newName);
            return message;
        };
    
        /**
         * Creates a plain object from a TaskReplyToRenameRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskReplyToRenameRequest
         * @static
         * @param {TaskReplyToRenameRequest} message TaskReplyToRenameRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskReplyToRenameRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.taskId = "";
                object.newName = "";
            }
            if (message.taskId != null && message.hasOwnProperty("taskId"))
                object.taskId = message.taskId;
            if (message.newName != null && message.hasOwnProperty("newName"))
                object.newName = message.newName;
            return object;
        };
    
        /**
         * Converts this TaskReplyToRenameRequest to JSON.
         * @function toJSON
         * @memberof TaskReplyToRenameRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskReplyToRenameRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskReplyToRenameRequest;
    })();
    
    $root.TaskReplyToRenameResponse = (function() {
    
        /**
         * Properties of a TaskReplyToRenameResponse.
         * @exports ITaskReplyToRenameResponse
         * @interface ITaskReplyToRenameResponse
         */
    
        /**
         * Constructs a new TaskReplyToRenameResponse.
         * @exports TaskReplyToRenameResponse
         * @classdesc Represents a TaskReplyToRenameResponse.
         * @implements ITaskReplyToRenameResponse
         * @constructor
         * @param {ITaskReplyToRenameResponse=} [properties] Properties to set
         */
        function TaskReplyToRenameResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new TaskReplyToRenameResponse instance using the specified properties.
         * @function create
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {ITaskReplyToRenameResponse=} [properties] Properties to set
         * @returns {TaskReplyToRenameResponse} TaskReplyToRenameResponse instance
         */
        TaskReplyToRenameResponse.create = function create(properties) {
            return new TaskReplyToRenameResponse(properties);
        };
    
        /**
         * Encodes the specified TaskReplyToRenameResponse message. Does not implicitly {@link TaskReplyToRenameResponse.verify|verify} messages.
         * @function encode
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {ITaskReplyToRenameResponse} message TaskReplyToRenameResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToRenameResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified TaskReplyToRenameResponse message, length delimited. Does not implicitly {@link TaskReplyToRenameResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {ITaskReplyToRenameResponse} message TaskReplyToRenameResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        TaskReplyToRenameResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a TaskReplyToRenameResponse message from the specified reader or buffer.
         * @function decode
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {TaskReplyToRenameResponse} TaskReplyToRenameResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToRenameResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.TaskReplyToRenameResponse();
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
         * Decodes a TaskReplyToRenameResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {TaskReplyToRenameResponse} TaskReplyToRenameResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        TaskReplyToRenameResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a TaskReplyToRenameResponse message.
         * @function verify
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        TaskReplyToRenameResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a TaskReplyToRenameResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {TaskReplyToRenameResponse} TaskReplyToRenameResponse
         */
        TaskReplyToRenameResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.TaskReplyToRenameResponse)
                return object;
            return new $root.TaskReplyToRenameResponse();
        };
    
        /**
         * Creates a plain object from a TaskReplyToRenameResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof TaskReplyToRenameResponse
         * @static
         * @param {TaskReplyToRenameResponse} message TaskReplyToRenameResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        TaskReplyToRenameResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this TaskReplyToRenameResponse to JSON.
         * @function toJSON
         * @memberof TaskReplyToRenameResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        TaskReplyToRenameResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return TaskReplyToRenameResponse;
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
