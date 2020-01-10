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
    
    $root.Keymap = (function() {
    
        /**
         * Properties of a Keymap.
         * @exports IKeymap
         * @interface IKeymap
         * @property {Array.<IBinding>|null} [bindings] Keymap bindings
         */
    
        /**
         * Constructs a new Keymap.
         * @exports Keymap
         * @classdesc Represents a Keymap.
         * @implements IKeymap
         * @constructor
         * @param {IKeymap=} [properties] Properties to set
         */
        function Keymap(properties) {
            this.bindings = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Keymap bindings.
         * @member {Array.<IBinding>} bindings
         * @memberof Keymap
         * @instance
         */
        Keymap.prototype.bindings = $util.emptyArray;
    
        /**
         * Creates a new Keymap instance using the specified properties.
         * @function create
         * @memberof Keymap
         * @static
         * @param {IKeymap=} [properties] Properties to set
         * @returns {Keymap} Keymap instance
         */
        Keymap.create = function create(properties) {
            return new Keymap(properties);
        };
    
        /**
         * Encodes the specified Keymap message. Does not implicitly {@link Keymap.verify|verify} messages.
         * @function encode
         * @memberof Keymap
         * @static
         * @param {IKeymap} message Keymap message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Keymap.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.bindings != null && message.bindings.length)
                for (var i = 0; i < message.bindings.length; ++i)
                    $root.Binding.encode(message.bindings[i], writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified Keymap message, length delimited. Does not implicitly {@link Keymap.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Keymap
         * @static
         * @param {IKeymap} message Keymap message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Keymap.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Keymap message from the specified reader or buffer.
         * @function decode
         * @memberof Keymap
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Keymap} Keymap
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Keymap.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Keymap();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    if (!(message.bindings && message.bindings.length))
                        message.bindings = [];
                    message.bindings.push($root.Binding.decode(reader, reader.uint32()));
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Keymap message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Keymap
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Keymap} Keymap
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Keymap.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Keymap message.
         * @function verify
         * @memberof Keymap
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Keymap.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.bindings != null && message.hasOwnProperty("bindings")) {
                if (!Array.isArray(message.bindings))
                    return "bindings: array expected";
                for (var i = 0; i < message.bindings.length; ++i) {
                    var error = $root.Binding.verify(message.bindings[i]);
                    if (error)
                        return "bindings." + error;
                }
            }
            return null;
        };
    
        /**
         * Creates a Keymap message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Keymap
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Keymap} Keymap
         */
        Keymap.fromObject = function fromObject(object) {
            if (object instanceof $root.Keymap)
                return object;
            var message = new $root.Keymap();
            if (object.bindings) {
                if (!Array.isArray(object.bindings))
                    throw TypeError(".Keymap.bindings: array expected");
                message.bindings = [];
                for (var i = 0; i < object.bindings.length; ++i) {
                    if (typeof object.bindings[i] !== "object")
                        throw TypeError(".Keymap.bindings: object expected");
                    message.bindings[i] = $root.Binding.fromObject(object.bindings[i]);
                }
            }
            return message;
        };
    
        /**
         * Creates a plain object from a Keymap message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Keymap
         * @static
         * @param {Keymap} message Keymap
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Keymap.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.bindings = [];
            if (message.bindings && message.bindings.length) {
                object.bindings = [];
                for (var j = 0; j < message.bindings.length; ++j)
                    object.bindings[j] = $root.Binding.toObject(message.bindings[j], options);
            }
            return object;
        };
    
        /**
         * Converts this Keymap to JSON.
         * @function toJSON
         * @memberof Keymap
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Keymap.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Keymap;
    })();
    
    $root.Binding = (function() {
    
        /**
         * Properties of a Binding.
         * @exports IBinding
         * @interface IBinding
         * @property {string|null} [key] Binding key
         * @property {string|null} [action] Binding action
         * @property {Array.<string>|null} [contexts] Binding contexts
         */
    
        /**
         * Constructs a new Binding.
         * @exports Binding
         * @classdesc Represents a Binding.
         * @implements IBinding
         * @constructor
         * @param {IBinding=} [properties] Properties to set
         */
        function Binding(properties) {
            this.contexts = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Binding key.
         * @member {string} key
         * @memberof Binding
         * @instance
         */
        Binding.prototype.key = "";
    
        /**
         * Binding action.
         * @member {string} action
         * @memberof Binding
         * @instance
         */
        Binding.prototype.action = "";
    
        /**
         * Binding contexts.
         * @member {Array.<string>} contexts
         * @memberof Binding
         * @instance
         */
        Binding.prototype.contexts = $util.emptyArray;
    
        /**
         * Creates a new Binding instance using the specified properties.
         * @function create
         * @memberof Binding
         * @static
         * @param {IBinding=} [properties] Properties to set
         * @returns {Binding} Binding instance
         */
        Binding.create = function create(properties) {
            return new Binding(properties);
        };
    
        /**
         * Encodes the specified Binding message. Does not implicitly {@link Binding.verify|verify} messages.
         * @function encode
         * @memberof Binding
         * @static
         * @param {IBinding} message Binding message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Binding.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.key != null && message.hasOwnProperty("key"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.key);
            if (message.action != null && message.hasOwnProperty("action"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.action);
            if (message.contexts != null && message.contexts.length)
                for (var i = 0; i < message.contexts.length; ++i)
                    writer.uint32(/* id 3, wireType 2 =*/26).string(message.contexts[i]);
            return writer;
        };
    
        /**
         * Encodes the specified Binding message, length delimited. Does not implicitly {@link Binding.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Binding
         * @static
         * @param {IBinding} message Binding message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Binding.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Binding message from the specified reader or buffer.
         * @function decode
         * @memberof Binding
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Binding} Binding
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Binding.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Binding();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.key = reader.string();
                    break;
                case 2:
                    message.action = reader.string();
                    break;
                case 3:
                    if (!(message.contexts && message.contexts.length))
                        message.contexts = [];
                    message.contexts.push(reader.string());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Binding message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Binding
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Binding} Binding
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Binding.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Binding message.
         * @function verify
         * @memberof Binding
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Binding.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.key != null && message.hasOwnProperty("key"))
                if (!$util.isString(message.key))
                    return "key: string expected";
            if (message.action != null && message.hasOwnProperty("action"))
                if (!$util.isString(message.action))
                    return "action: string expected";
            if (message.contexts != null && message.hasOwnProperty("contexts")) {
                if (!Array.isArray(message.contexts))
                    return "contexts: array expected";
                for (var i = 0; i < message.contexts.length; ++i)
                    if (!$util.isString(message.contexts[i]))
                        return "contexts: string[] expected";
            }
            return null;
        };
    
        /**
         * Creates a Binding message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Binding
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Binding} Binding
         */
        Binding.fromObject = function fromObject(object) {
            if (object instanceof $root.Binding)
                return object;
            var message = new $root.Binding();
            if (object.key != null)
                message.key = String(object.key);
            if (object.action != null)
                message.action = String(object.action);
            if (object.contexts) {
                if (!Array.isArray(object.contexts))
                    throw TypeError(".Binding.contexts: array expected");
                message.contexts = [];
                for (var i = 0; i < object.contexts.length; ++i)
                    message.contexts[i] = String(object.contexts[i]);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a Binding message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Binding
         * @static
         * @param {Binding} message Binding
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Binding.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.contexts = [];
            if (options.defaults) {
                object.key = "";
                object.action = "";
            }
            if (message.key != null && message.hasOwnProperty("key"))
                object.key = message.key;
            if (message.action != null && message.hasOwnProperty("action"))
                object.action = message.action;
            if (message.contexts && message.contexts.length) {
                object.contexts = [];
                for (var j = 0; j < message.contexts.length; ++j)
                    object.contexts[j] = message.contexts[j];
            }
            return object;
        };
    
        /**
         * Converts this Binding to JSON.
         * @function toJSON
         * @memberof Binding
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Binding.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Binding;
    })();
    
    $root.KeymapGetRequest = (function() {
    
        /**
         * Properties of a KeymapGetRequest.
         * @exports IKeymapGetRequest
         * @interface IKeymapGetRequest
         */
    
        /**
         * Constructs a new KeymapGetRequest.
         * @exports KeymapGetRequest
         * @classdesc Represents a KeymapGetRequest.
         * @implements IKeymapGetRequest
         * @constructor
         * @param {IKeymapGetRequest=} [properties] Properties to set
         */
        function KeymapGetRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new KeymapGetRequest instance using the specified properties.
         * @function create
         * @memberof KeymapGetRequest
         * @static
         * @param {IKeymapGetRequest=} [properties] Properties to set
         * @returns {KeymapGetRequest} KeymapGetRequest instance
         */
        KeymapGetRequest.create = function create(properties) {
            return new KeymapGetRequest(properties);
        };
    
        /**
         * Encodes the specified KeymapGetRequest message. Does not implicitly {@link KeymapGetRequest.verify|verify} messages.
         * @function encode
         * @memberof KeymapGetRequest
         * @static
         * @param {IKeymapGetRequest} message KeymapGetRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapGetRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified KeymapGetRequest message, length delimited. Does not implicitly {@link KeymapGetRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof KeymapGetRequest
         * @static
         * @param {IKeymapGetRequest} message KeymapGetRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapGetRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a KeymapGetRequest message from the specified reader or buffer.
         * @function decode
         * @memberof KeymapGetRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {KeymapGetRequest} KeymapGetRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapGetRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.KeymapGetRequest();
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
         * Decodes a KeymapGetRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof KeymapGetRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {KeymapGetRequest} KeymapGetRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapGetRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a KeymapGetRequest message.
         * @function verify
         * @memberof KeymapGetRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        KeymapGetRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a KeymapGetRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof KeymapGetRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {KeymapGetRequest} KeymapGetRequest
         */
        KeymapGetRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.KeymapGetRequest)
                return object;
            return new $root.KeymapGetRequest();
        };
    
        /**
         * Creates a plain object from a KeymapGetRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof KeymapGetRequest
         * @static
         * @param {KeymapGetRequest} message KeymapGetRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        KeymapGetRequest.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this KeymapGetRequest to JSON.
         * @function toJSON
         * @memberof KeymapGetRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        KeymapGetRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return KeymapGetRequest;
    })();
    
    $root.KeymapGetResponse = (function() {
    
        /**
         * Properties of a KeymapGetResponse.
         * @exports IKeymapGetResponse
         * @interface IKeymapGetResponse
         * @property {IKeymap|null} [keymap] KeymapGetResponse keymap
         */
    
        /**
         * Constructs a new KeymapGetResponse.
         * @exports KeymapGetResponse
         * @classdesc Represents a KeymapGetResponse.
         * @implements IKeymapGetResponse
         * @constructor
         * @param {IKeymapGetResponse=} [properties] Properties to set
         */
        function KeymapGetResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * KeymapGetResponse keymap.
         * @member {IKeymap|null|undefined} keymap
         * @memberof KeymapGetResponse
         * @instance
         */
        KeymapGetResponse.prototype.keymap = null;
    
        /**
         * Creates a new KeymapGetResponse instance using the specified properties.
         * @function create
         * @memberof KeymapGetResponse
         * @static
         * @param {IKeymapGetResponse=} [properties] Properties to set
         * @returns {KeymapGetResponse} KeymapGetResponse instance
         */
        KeymapGetResponse.create = function create(properties) {
            return new KeymapGetResponse(properties);
        };
    
        /**
         * Encodes the specified KeymapGetResponse message. Does not implicitly {@link KeymapGetResponse.verify|verify} messages.
         * @function encode
         * @memberof KeymapGetResponse
         * @static
         * @param {IKeymapGetResponse} message KeymapGetResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapGetResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.keymap != null && message.hasOwnProperty("keymap"))
                $root.Keymap.encode(message.keymap, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified KeymapGetResponse message, length delimited. Does not implicitly {@link KeymapGetResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof KeymapGetResponse
         * @static
         * @param {IKeymapGetResponse} message KeymapGetResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapGetResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a KeymapGetResponse message from the specified reader or buffer.
         * @function decode
         * @memberof KeymapGetResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {KeymapGetResponse} KeymapGetResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapGetResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.KeymapGetResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.keymap = $root.Keymap.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a KeymapGetResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof KeymapGetResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {KeymapGetResponse} KeymapGetResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapGetResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a KeymapGetResponse message.
         * @function verify
         * @memberof KeymapGetResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        KeymapGetResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.keymap != null && message.hasOwnProperty("keymap")) {
                var error = $root.Keymap.verify(message.keymap);
                if (error)
                    return "keymap." + error;
            }
            return null;
        };
    
        /**
         * Creates a KeymapGetResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof KeymapGetResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {KeymapGetResponse} KeymapGetResponse
         */
        KeymapGetResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.KeymapGetResponse)
                return object;
            var message = new $root.KeymapGetResponse();
            if (object.keymap != null) {
                if (typeof object.keymap !== "object")
                    throw TypeError(".KeymapGetResponse.keymap: object expected");
                message.keymap = $root.Keymap.fromObject(object.keymap);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a KeymapGetResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof KeymapGetResponse
         * @static
         * @param {KeymapGetResponse} message KeymapGetResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        KeymapGetResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.keymap = null;
            if (message.keymap != null && message.hasOwnProperty("keymap"))
                object.keymap = $root.Keymap.toObject(message.keymap, options);
            return object;
        };
    
        /**
         * Converts this KeymapGetResponse to JSON.
         * @function toJSON
         * @memberof KeymapGetResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        KeymapGetResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return KeymapGetResponse;
    })();
    
    $root.KeymapReloadRequest = (function() {
    
        /**
         * Properties of a KeymapReloadRequest.
         * @exports IKeymapReloadRequest
         * @interface IKeymapReloadRequest
         */
    
        /**
         * Constructs a new KeymapReloadRequest.
         * @exports KeymapReloadRequest
         * @classdesc Represents a KeymapReloadRequest.
         * @implements IKeymapReloadRequest
         * @constructor
         * @param {IKeymapReloadRequest=} [properties] Properties to set
         */
        function KeymapReloadRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new KeymapReloadRequest instance using the specified properties.
         * @function create
         * @memberof KeymapReloadRequest
         * @static
         * @param {IKeymapReloadRequest=} [properties] Properties to set
         * @returns {KeymapReloadRequest} KeymapReloadRequest instance
         */
        KeymapReloadRequest.create = function create(properties) {
            return new KeymapReloadRequest(properties);
        };
    
        /**
         * Encodes the specified KeymapReloadRequest message. Does not implicitly {@link KeymapReloadRequest.verify|verify} messages.
         * @function encode
         * @memberof KeymapReloadRequest
         * @static
         * @param {IKeymapReloadRequest} message KeymapReloadRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapReloadRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified KeymapReloadRequest message, length delimited. Does not implicitly {@link KeymapReloadRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof KeymapReloadRequest
         * @static
         * @param {IKeymapReloadRequest} message KeymapReloadRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapReloadRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a KeymapReloadRequest message from the specified reader or buffer.
         * @function decode
         * @memberof KeymapReloadRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {KeymapReloadRequest} KeymapReloadRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapReloadRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.KeymapReloadRequest();
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
         * Decodes a KeymapReloadRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof KeymapReloadRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {KeymapReloadRequest} KeymapReloadRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapReloadRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a KeymapReloadRequest message.
         * @function verify
         * @memberof KeymapReloadRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        KeymapReloadRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a KeymapReloadRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof KeymapReloadRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {KeymapReloadRequest} KeymapReloadRequest
         */
        KeymapReloadRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.KeymapReloadRequest)
                return object;
            return new $root.KeymapReloadRequest();
        };
    
        /**
         * Creates a plain object from a KeymapReloadRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof KeymapReloadRequest
         * @static
         * @param {KeymapReloadRequest} message KeymapReloadRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        KeymapReloadRequest.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this KeymapReloadRequest to JSON.
         * @function toJSON
         * @memberof KeymapReloadRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        KeymapReloadRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return KeymapReloadRequest;
    })();
    
    $root.KeymapReloadResponse = (function() {
    
        /**
         * Properties of a KeymapReloadResponse.
         * @exports IKeymapReloadResponse
         * @interface IKeymapReloadResponse
         * @property {IKeymap|null} [keymap] KeymapReloadResponse keymap
         */
    
        /**
         * Constructs a new KeymapReloadResponse.
         * @exports KeymapReloadResponse
         * @classdesc Represents a KeymapReloadResponse.
         * @implements IKeymapReloadResponse
         * @constructor
         * @param {IKeymapReloadResponse=} [properties] Properties to set
         */
        function KeymapReloadResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * KeymapReloadResponse keymap.
         * @member {IKeymap|null|undefined} keymap
         * @memberof KeymapReloadResponse
         * @instance
         */
        KeymapReloadResponse.prototype.keymap = null;
    
        /**
         * Creates a new KeymapReloadResponse instance using the specified properties.
         * @function create
         * @memberof KeymapReloadResponse
         * @static
         * @param {IKeymapReloadResponse=} [properties] Properties to set
         * @returns {KeymapReloadResponse} KeymapReloadResponse instance
         */
        KeymapReloadResponse.create = function create(properties) {
            return new KeymapReloadResponse(properties);
        };
    
        /**
         * Encodes the specified KeymapReloadResponse message. Does not implicitly {@link KeymapReloadResponse.verify|verify} messages.
         * @function encode
         * @memberof KeymapReloadResponse
         * @static
         * @param {IKeymapReloadResponse} message KeymapReloadResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapReloadResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.keymap != null && message.hasOwnProperty("keymap"))
                $root.Keymap.encode(message.keymap, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified KeymapReloadResponse message, length delimited. Does not implicitly {@link KeymapReloadResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof KeymapReloadResponse
         * @static
         * @param {IKeymapReloadResponse} message KeymapReloadResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapReloadResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a KeymapReloadResponse message from the specified reader or buffer.
         * @function decode
         * @memberof KeymapReloadResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {KeymapReloadResponse} KeymapReloadResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapReloadResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.KeymapReloadResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.keymap = $root.Keymap.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a KeymapReloadResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof KeymapReloadResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {KeymapReloadResponse} KeymapReloadResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapReloadResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a KeymapReloadResponse message.
         * @function verify
         * @memberof KeymapReloadResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        KeymapReloadResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.keymap != null && message.hasOwnProperty("keymap")) {
                var error = $root.Keymap.verify(message.keymap);
                if (error)
                    return "keymap." + error;
            }
            return null;
        };
    
        /**
         * Creates a KeymapReloadResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof KeymapReloadResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {KeymapReloadResponse} KeymapReloadResponse
         */
        KeymapReloadResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.KeymapReloadResponse)
                return object;
            var message = new $root.KeymapReloadResponse();
            if (object.keymap != null) {
                if (typeof object.keymap !== "object")
                    throw TypeError(".KeymapReloadResponse.keymap: object expected");
                message.keymap = $root.Keymap.fromObject(object.keymap);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a KeymapReloadResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof KeymapReloadResponse
         * @static
         * @param {KeymapReloadResponse} message KeymapReloadResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        KeymapReloadResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.keymap = null;
            if (message.keymap != null && message.hasOwnProperty("keymap"))
                object.keymap = $root.Keymap.toObject(message.keymap, options);
            return object;
        };
    
        /**
         * Converts this KeymapReloadResponse to JSON.
         * @function toJSON
         * @memberof KeymapReloadResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        KeymapReloadResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return KeymapReloadResponse;
    })();
    
    $root.KeymapStoreRequest = (function() {
    
        /**
         * Properties of a KeymapStoreRequest.
         * @exports IKeymapStoreRequest
         * @interface IKeymapStoreRequest
         * @property {IKeymap|null} [keymap] KeymapStoreRequest keymap
         */
    
        /**
         * Constructs a new KeymapStoreRequest.
         * @exports KeymapStoreRequest
         * @classdesc Represents a KeymapStoreRequest.
         * @implements IKeymapStoreRequest
         * @constructor
         * @param {IKeymapStoreRequest=} [properties] Properties to set
         */
        function KeymapStoreRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * KeymapStoreRequest keymap.
         * @member {IKeymap|null|undefined} keymap
         * @memberof KeymapStoreRequest
         * @instance
         */
        KeymapStoreRequest.prototype.keymap = null;
    
        /**
         * Creates a new KeymapStoreRequest instance using the specified properties.
         * @function create
         * @memberof KeymapStoreRequest
         * @static
         * @param {IKeymapStoreRequest=} [properties] Properties to set
         * @returns {KeymapStoreRequest} KeymapStoreRequest instance
         */
        KeymapStoreRequest.create = function create(properties) {
            return new KeymapStoreRequest(properties);
        };
    
        /**
         * Encodes the specified KeymapStoreRequest message. Does not implicitly {@link KeymapStoreRequest.verify|verify} messages.
         * @function encode
         * @memberof KeymapStoreRequest
         * @static
         * @param {IKeymapStoreRequest} message KeymapStoreRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapStoreRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.keymap != null && message.hasOwnProperty("keymap"))
                $root.Keymap.encode(message.keymap, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified KeymapStoreRequest message, length delimited. Does not implicitly {@link KeymapStoreRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof KeymapStoreRequest
         * @static
         * @param {IKeymapStoreRequest} message KeymapStoreRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapStoreRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a KeymapStoreRequest message from the specified reader or buffer.
         * @function decode
         * @memberof KeymapStoreRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {KeymapStoreRequest} KeymapStoreRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapStoreRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.KeymapStoreRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.keymap = $root.Keymap.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a KeymapStoreRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof KeymapStoreRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {KeymapStoreRequest} KeymapStoreRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapStoreRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a KeymapStoreRequest message.
         * @function verify
         * @memberof KeymapStoreRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        KeymapStoreRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.keymap != null && message.hasOwnProperty("keymap")) {
                var error = $root.Keymap.verify(message.keymap);
                if (error)
                    return "keymap." + error;
            }
            return null;
        };
    
        /**
         * Creates a KeymapStoreRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof KeymapStoreRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {KeymapStoreRequest} KeymapStoreRequest
         */
        KeymapStoreRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.KeymapStoreRequest)
                return object;
            var message = new $root.KeymapStoreRequest();
            if (object.keymap != null) {
                if (typeof object.keymap !== "object")
                    throw TypeError(".KeymapStoreRequest.keymap: object expected");
                message.keymap = $root.Keymap.fromObject(object.keymap);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a KeymapStoreRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof KeymapStoreRequest
         * @static
         * @param {KeymapStoreRequest} message KeymapStoreRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        KeymapStoreRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.keymap = null;
            if (message.keymap != null && message.hasOwnProperty("keymap"))
                object.keymap = $root.Keymap.toObject(message.keymap, options);
            return object;
        };
    
        /**
         * Converts this KeymapStoreRequest to JSON.
         * @function toJSON
         * @memberof KeymapStoreRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        KeymapStoreRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return KeymapStoreRequest;
    })();
    
    $root.KeymapStoreResponse = (function() {
    
        /**
         * Properties of a KeymapStoreResponse.
         * @exports IKeymapStoreResponse
         * @interface IKeymapStoreResponse
         */
    
        /**
         * Constructs a new KeymapStoreResponse.
         * @exports KeymapStoreResponse
         * @classdesc Represents a KeymapStoreResponse.
         * @implements IKeymapStoreResponse
         * @constructor
         * @param {IKeymapStoreResponse=} [properties] Properties to set
         */
        function KeymapStoreResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new KeymapStoreResponse instance using the specified properties.
         * @function create
         * @memberof KeymapStoreResponse
         * @static
         * @param {IKeymapStoreResponse=} [properties] Properties to set
         * @returns {KeymapStoreResponse} KeymapStoreResponse instance
         */
        KeymapStoreResponse.create = function create(properties) {
            return new KeymapStoreResponse(properties);
        };
    
        /**
         * Encodes the specified KeymapStoreResponse message. Does not implicitly {@link KeymapStoreResponse.verify|verify} messages.
         * @function encode
         * @memberof KeymapStoreResponse
         * @static
         * @param {IKeymapStoreResponse} message KeymapStoreResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapStoreResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified KeymapStoreResponse message, length delimited. Does not implicitly {@link KeymapStoreResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof KeymapStoreResponse
         * @static
         * @param {IKeymapStoreResponse} message KeymapStoreResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        KeymapStoreResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a KeymapStoreResponse message from the specified reader or buffer.
         * @function decode
         * @memberof KeymapStoreResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {KeymapStoreResponse} KeymapStoreResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapStoreResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.KeymapStoreResponse();
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
         * Decodes a KeymapStoreResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof KeymapStoreResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {KeymapStoreResponse} KeymapStoreResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        KeymapStoreResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a KeymapStoreResponse message.
         * @function verify
         * @memberof KeymapStoreResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        KeymapStoreResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a KeymapStoreResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof KeymapStoreResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {KeymapStoreResponse} KeymapStoreResponse
         */
        KeymapStoreResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.KeymapStoreResponse)
                return object;
            return new $root.KeymapStoreResponse();
        };
    
        /**
         * Creates a plain object from a KeymapStoreResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof KeymapStoreResponse
         * @static
         * @param {KeymapStoreResponse} message KeymapStoreResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        KeymapStoreResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this KeymapStoreResponse to JSON.
         * @function toJSON
         * @memberof KeymapStoreResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        KeymapStoreResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return KeymapStoreResponse;
    })();

    return $root;
});
