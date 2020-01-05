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
    
    $root.Item = (function() {
    
        /**
         * Properties of an Item.
         * @exports IItem
         * @interface IItem
         * @property {string|null} [id] Item id
         * @property {string|null} [value] Item value
         */
    
        /**
         * Constructs a new Item.
         * @exports Item
         * @classdesc Represents an Item.
         * @implements IItem
         * @constructor
         * @param {IItem=} [properties] Properties to set
         */
        function Item(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Item id.
         * @member {string} id
         * @memberof Item
         * @instance
         */
        Item.prototype.id = "";
    
        /**
         * Item value.
         * @member {string} value
         * @memberof Item
         * @instance
         */
        Item.prototype.value = "";
    
        /**
         * Creates a new Item instance using the specified properties.
         * @function create
         * @memberof Item
         * @static
         * @param {IItem=} [properties] Properties to set
         * @returns {Item} Item instance
         */
        Item.create = function create(properties) {
            return new Item(properties);
        };
    
        /**
         * Encodes the specified Item message. Does not implicitly {@link Item.verify|verify} messages.
         * @function encode
         * @memberof Item
         * @static
         * @param {IItem} message Item message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Item.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.id != null && message.hasOwnProperty("id"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.id);
            if (message.value != null && message.hasOwnProperty("value"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.value);
            return writer;
        };
    
        /**
         * Encodes the specified Item message, length delimited. Does not implicitly {@link Item.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Item
         * @static
         * @param {IItem} message Item message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Item.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes an Item message from the specified reader or buffer.
         * @function decode
         * @memberof Item
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Item} Item
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Item.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Item();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.id = reader.string();
                    break;
                case 2:
                    message.value = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes an Item message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Item
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Item} Item
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Item.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies an Item message.
         * @function verify
         * @memberof Item
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Item.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.id != null && message.hasOwnProperty("id"))
                if (!$util.isString(message.id))
                    return "id: string expected";
            if (message.value != null && message.hasOwnProperty("value"))
                if (!$util.isString(message.value))
                    return "value: string expected";
            return null;
        };
    
        /**
         * Creates an Item message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Item
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Item} Item
         */
        Item.fromObject = function fromObject(object) {
            if (object instanceof $root.Item)
                return object;
            var message = new $root.Item();
            if (object.id != null)
                message.id = String(object.id);
            if (object.value != null)
                message.value = String(object.value);
            return message;
        };
    
        /**
         * Creates a plain object from an Item message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Item
         * @static
         * @param {Item} message Item
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Item.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.id = "";
                object.value = "";
            }
            if (message.id != null && message.hasOwnProperty("id"))
                object.id = message.id;
            if (message.value != null && message.hasOwnProperty("value"))
                object.value = message.value;
            return object;
        };
    
        /**
         * Converts this Item to JSON.
         * @function toJSON
         * @memberof Item
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Item.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Item;
    })();
    
    $root.Candidate = (function() {
    
        /**
         * Properties of a Candidate.
         * @exports ICandidate
         * @interface ICandidate
         * @property {number|null} [start] Candidate start
         * @property {number|null} [length] Candidate length
         * @property {IItem|null} [value] Candidate value
         */
    
        /**
         * Constructs a new Candidate.
         * @exports Candidate
         * @classdesc Represents a Candidate.
         * @implements ICandidate
         * @constructor
         * @param {ICandidate=} [properties] Properties to set
         */
        function Candidate(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Candidate start.
         * @member {number} start
         * @memberof Candidate
         * @instance
         */
        Candidate.prototype.start = 0;
    
        /**
         * Candidate length.
         * @member {number} length
         * @memberof Candidate
         * @instance
         */
        Candidate.prototype.length = 0;
    
        /**
         * Candidate value.
         * @member {IItem|null|undefined} value
         * @memberof Candidate
         * @instance
         */
        Candidate.prototype.value = null;
    
        /**
         * Creates a new Candidate instance using the specified properties.
         * @function create
         * @memberof Candidate
         * @static
         * @param {ICandidate=} [properties] Properties to set
         * @returns {Candidate} Candidate instance
         */
        Candidate.create = function create(properties) {
            return new Candidate(properties);
        };
    
        /**
         * Encodes the specified Candidate message. Does not implicitly {@link Candidate.verify|verify} messages.
         * @function encode
         * @memberof Candidate
         * @static
         * @param {ICandidate} message Candidate message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Candidate.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.start != null && message.hasOwnProperty("start"))
                writer.uint32(/* id 1, wireType 0 =*/8).int32(message.start);
            if (message.length != null && message.hasOwnProperty("length"))
                writer.uint32(/* id 2, wireType 0 =*/16).int32(message.length);
            if (message.value != null && message.hasOwnProperty("value"))
                $root.Item.encode(message.value, writer.uint32(/* id 3, wireType 2 =*/26).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified Candidate message, length delimited. Does not implicitly {@link Candidate.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Candidate
         * @static
         * @param {ICandidate} message Candidate message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Candidate.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Candidate message from the specified reader or buffer.
         * @function decode
         * @memberof Candidate
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Candidate} Candidate
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Candidate.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Candidate();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.start = reader.int32();
                    break;
                case 2:
                    message.length = reader.int32();
                    break;
                case 3:
                    message.value = $root.Item.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Candidate message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Candidate
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Candidate} Candidate
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Candidate.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Candidate message.
         * @function verify
         * @memberof Candidate
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Candidate.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.start != null && message.hasOwnProperty("start"))
                if (!$util.isInteger(message.start))
                    return "start: integer expected";
            if (message.length != null && message.hasOwnProperty("length"))
                if (!$util.isInteger(message.length))
                    return "length: integer expected";
            if (message.value != null && message.hasOwnProperty("value")) {
                var error = $root.Item.verify(message.value);
                if (error)
                    return "value." + error;
            }
            return null;
        };
    
        /**
         * Creates a Candidate message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Candidate
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Candidate} Candidate
         */
        Candidate.fromObject = function fromObject(object) {
            if (object instanceof $root.Candidate)
                return object;
            var message = new $root.Candidate();
            if (object.start != null)
                message.start = object.start | 0;
            if (object.length != null)
                message.length = object.length | 0;
            if (object.value != null) {
                if (typeof object.value !== "object")
                    throw TypeError(".Candidate.value: object expected");
                message.value = $root.Item.fromObject(object.value);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a Candidate message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Candidate
         * @static
         * @param {Candidate} message Candidate
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Candidate.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.start = 0;
                object.length = 0;
                object.value = null;
            }
            if (message.start != null && message.hasOwnProperty("start"))
                object.start = message.start;
            if (message.length != null && message.hasOwnProperty("length"))
                object.length = message.length;
            if (message.value != null && message.hasOwnProperty("value"))
                object.value = $root.Item.toObject(message.value, options);
            return object;
        };
    
        /**
         * Converts this Candidate to JSON.
         * @function toJSON
         * @memberof Candidate
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Candidate.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Candidate;
    })();
    
    $root.SetupRequest = (function() {
    
        /**
         * Properties of a SetupRequest.
         * @exports ISetupRequest
         * @interface ISetupRequest
         * @property {Array.<IItem>|null} [source] SetupRequest source
         */
    
        /**
         * Constructs a new SetupRequest.
         * @exports SetupRequest
         * @classdesc Represents a SetupRequest.
         * @implements ISetupRequest
         * @constructor
         * @param {ISetupRequest=} [properties] Properties to set
         */
        function SetupRequest(properties) {
            this.source = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * SetupRequest source.
         * @member {Array.<IItem>} source
         * @memberof SetupRequest
         * @instance
         */
        SetupRequest.prototype.source = $util.emptyArray;
    
        /**
         * Creates a new SetupRequest instance using the specified properties.
         * @function create
         * @memberof SetupRequest
         * @static
         * @param {ISetupRequest=} [properties] Properties to set
         * @returns {SetupRequest} SetupRequest instance
         */
        SetupRequest.create = function create(properties) {
            return new SetupRequest(properties);
        };
    
        /**
         * Encodes the specified SetupRequest message. Does not implicitly {@link SetupRequest.verify|verify} messages.
         * @function encode
         * @memberof SetupRequest
         * @static
         * @param {ISetupRequest} message SetupRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        SetupRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.source != null && message.source.length)
                for (var i = 0; i < message.source.length; ++i)
                    $root.Item.encode(message.source[i], writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified SetupRequest message, length delimited. Does not implicitly {@link SetupRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof SetupRequest
         * @static
         * @param {ISetupRequest} message SetupRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        SetupRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a SetupRequest message from the specified reader or buffer.
         * @function decode
         * @memberof SetupRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {SetupRequest} SetupRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        SetupRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.SetupRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    if (!(message.source && message.source.length))
                        message.source = [];
                    message.source.push($root.Item.decode(reader, reader.uint32()));
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a SetupRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof SetupRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {SetupRequest} SetupRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        SetupRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a SetupRequest message.
         * @function verify
         * @memberof SetupRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        SetupRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.source != null && message.hasOwnProperty("source")) {
                if (!Array.isArray(message.source))
                    return "source: array expected";
                for (var i = 0; i < message.source.length; ++i) {
                    var error = $root.Item.verify(message.source[i]);
                    if (error)
                        return "source." + error;
                }
            }
            return null;
        };
    
        /**
         * Creates a SetupRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof SetupRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {SetupRequest} SetupRequest
         */
        SetupRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.SetupRequest)
                return object;
            var message = new $root.SetupRequest();
            if (object.source) {
                if (!Array.isArray(object.source))
                    throw TypeError(".SetupRequest.source: array expected");
                message.source = [];
                for (var i = 0; i < object.source.length; ++i) {
                    if (typeof object.source[i] !== "object")
                        throw TypeError(".SetupRequest.source: object expected");
                    message.source[i] = $root.Item.fromObject(object.source[i]);
                }
            }
            return message;
        };
    
        /**
         * Creates a plain object from a SetupRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof SetupRequest
         * @static
         * @param {SetupRequest} message SetupRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        SetupRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.source = [];
            if (message.source && message.source.length) {
                object.source = [];
                for (var j = 0; j < message.source.length; ++j)
                    object.source[j] = $root.Item.toObject(message.source[j], options);
            }
            return object;
        };
    
        /**
         * Converts this SetupRequest to JSON.
         * @function toJSON
         * @memberof SetupRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        SetupRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return SetupRequest;
    })();
    
    $root.SetupResponse = (function() {
    
        /**
         * Properties of a SetupResponse.
         * @exports ISetupResponse
         * @interface ISetupResponse
         */
    
        /**
         * Constructs a new SetupResponse.
         * @exports SetupResponse
         * @classdesc Represents a SetupResponse.
         * @implements ISetupResponse
         * @constructor
         * @param {ISetupResponse=} [properties] Properties to set
         */
        function SetupResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new SetupResponse instance using the specified properties.
         * @function create
         * @memberof SetupResponse
         * @static
         * @param {ISetupResponse=} [properties] Properties to set
         * @returns {SetupResponse} SetupResponse instance
         */
        SetupResponse.create = function create(properties) {
            return new SetupResponse(properties);
        };
    
        /**
         * Encodes the specified SetupResponse message. Does not implicitly {@link SetupResponse.verify|verify} messages.
         * @function encode
         * @memberof SetupResponse
         * @static
         * @param {ISetupResponse} message SetupResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        SetupResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified SetupResponse message, length delimited. Does not implicitly {@link SetupResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof SetupResponse
         * @static
         * @param {ISetupResponse} message SetupResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        SetupResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a SetupResponse message from the specified reader or buffer.
         * @function decode
         * @memberof SetupResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {SetupResponse} SetupResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        SetupResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.SetupResponse();
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
         * Decodes a SetupResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof SetupResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {SetupResponse} SetupResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        SetupResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a SetupResponse message.
         * @function verify
         * @memberof SetupResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        SetupResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a SetupResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof SetupResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {SetupResponse} SetupResponse
         */
        SetupResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.SetupResponse)
                return object;
            return new $root.SetupResponse();
        };
    
        /**
         * Creates a plain object from a SetupResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof SetupResponse
         * @static
         * @param {SetupResponse} message SetupResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        SetupResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this SetupResponse to JSON.
         * @function toJSON
         * @memberof SetupResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        SetupResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return SetupResponse;
    })();
    
    $root.ReadRequest = (function() {
    
        /**
         * Properties of a ReadRequest.
         * @exports IReadRequest
         * @interface IReadRequest
         * @property {string|null} [input] ReadRequest input
         */
    
        /**
         * Constructs a new ReadRequest.
         * @exports ReadRequest
         * @classdesc Represents a ReadRequest.
         * @implements IReadRequest
         * @constructor
         * @param {IReadRequest=} [properties] Properties to set
         */
        function ReadRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * ReadRequest input.
         * @member {string} input
         * @memberof ReadRequest
         * @instance
         */
        ReadRequest.prototype.input = "";
    
        /**
         * Creates a new ReadRequest instance using the specified properties.
         * @function create
         * @memberof ReadRequest
         * @static
         * @param {IReadRequest=} [properties] Properties to set
         * @returns {ReadRequest} ReadRequest instance
         */
        ReadRequest.create = function create(properties) {
            return new ReadRequest(properties);
        };
    
        /**
         * Encodes the specified ReadRequest message. Does not implicitly {@link ReadRequest.verify|verify} messages.
         * @function encode
         * @memberof ReadRequest
         * @static
         * @param {IReadRequest} message ReadRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ReadRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.input != null && message.hasOwnProperty("input"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.input);
            return writer;
        };
    
        /**
         * Encodes the specified ReadRequest message, length delimited. Does not implicitly {@link ReadRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof ReadRequest
         * @static
         * @param {IReadRequest} message ReadRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ReadRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a ReadRequest message from the specified reader or buffer.
         * @function decode
         * @memberof ReadRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {ReadRequest} ReadRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ReadRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.ReadRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.input = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a ReadRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof ReadRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {ReadRequest} ReadRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ReadRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a ReadRequest message.
         * @function verify
         * @memberof ReadRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        ReadRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.input != null && message.hasOwnProperty("input"))
                if (!$util.isString(message.input))
                    return "input: string expected";
            return null;
        };
    
        /**
         * Creates a ReadRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof ReadRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {ReadRequest} ReadRequest
         */
        ReadRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.ReadRequest)
                return object;
            var message = new $root.ReadRequest();
            if (object.input != null)
                message.input = String(object.input);
            return message;
        };
    
        /**
         * Creates a plain object from a ReadRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof ReadRequest
         * @static
         * @param {ReadRequest} message ReadRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        ReadRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.input = "";
            if (message.input != null && message.hasOwnProperty("input"))
                object.input = message.input;
            return object;
        };
    
        /**
         * Converts this ReadRequest to JSON.
         * @function toJSON
         * @memberof ReadRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        ReadRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return ReadRequest;
    })();
    
    $root.ReadResponse = (function() {
    
        /**
         * Properties of a ReadResponse.
         * @exports IReadResponse
         * @interface IReadResponse
         * @property {Array.<ICandidate>|null} [candidates] ReadResponse candidates
         */
    
        /**
         * Constructs a new ReadResponse.
         * @exports ReadResponse
         * @classdesc Represents a ReadResponse.
         * @implements IReadResponse
         * @constructor
         * @param {IReadResponse=} [properties] Properties to set
         */
        function ReadResponse(properties) {
            this.candidates = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * ReadResponse candidates.
         * @member {Array.<ICandidate>} candidates
         * @memberof ReadResponse
         * @instance
         */
        ReadResponse.prototype.candidates = $util.emptyArray;
    
        /**
         * Creates a new ReadResponse instance using the specified properties.
         * @function create
         * @memberof ReadResponse
         * @static
         * @param {IReadResponse=} [properties] Properties to set
         * @returns {ReadResponse} ReadResponse instance
         */
        ReadResponse.create = function create(properties) {
            return new ReadResponse(properties);
        };
    
        /**
         * Encodes the specified ReadResponse message. Does not implicitly {@link ReadResponse.verify|verify} messages.
         * @function encode
         * @memberof ReadResponse
         * @static
         * @param {IReadResponse} message ReadResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ReadResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.candidates != null && message.candidates.length)
                for (var i = 0; i < message.candidates.length; ++i)
                    $root.Candidate.encode(message.candidates[i], writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified ReadResponse message, length delimited. Does not implicitly {@link ReadResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof ReadResponse
         * @static
         * @param {IReadResponse} message ReadResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ReadResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a ReadResponse message from the specified reader or buffer.
         * @function decode
         * @memberof ReadResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {ReadResponse} ReadResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ReadResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.ReadResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    if (!(message.candidates && message.candidates.length))
                        message.candidates = [];
                    message.candidates.push($root.Candidate.decode(reader, reader.uint32()));
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a ReadResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof ReadResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {ReadResponse} ReadResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ReadResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a ReadResponse message.
         * @function verify
         * @memberof ReadResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        ReadResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.candidates != null && message.hasOwnProperty("candidates")) {
                if (!Array.isArray(message.candidates))
                    return "candidates: array expected";
                for (var i = 0; i < message.candidates.length; ++i) {
                    var error = $root.Candidate.verify(message.candidates[i]);
                    if (error)
                        return "candidates." + error;
                }
            }
            return null;
        };
    
        /**
         * Creates a ReadResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof ReadResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {ReadResponse} ReadResponse
         */
        ReadResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.ReadResponse)
                return object;
            var message = new $root.ReadResponse();
            if (object.candidates) {
                if (!Array.isArray(object.candidates))
                    throw TypeError(".ReadResponse.candidates: array expected");
                message.candidates = [];
                for (var i = 0; i < object.candidates.length; ++i) {
                    if (typeof object.candidates[i] !== "object")
                        throw TypeError(".ReadResponse.candidates: object expected");
                    message.candidates[i] = $root.Candidate.fromObject(object.candidates[i]);
                }
            }
            return message;
        };
    
        /**
         * Creates a plain object from a ReadResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof ReadResponse
         * @static
         * @param {ReadResponse} message ReadResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        ReadResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.candidates = [];
            if (message.candidates && message.candidates.length) {
                object.candidates = [];
                for (var j = 0; j < message.candidates.length; ++j)
                    object.candidates[j] = $root.Candidate.toObject(message.candidates[j], options);
            }
            return object;
        };
    
        /**
         * Converts this ReadResponse to JSON.
         * @function toJSON
         * @memberof ReadResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        ReadResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return ReadResponse;
    })();

    return $root;
});
