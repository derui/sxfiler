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
    
    $root.Configuration = (function() {
    
        /**
         * Properties of a Configuration.
         * @exports IConfiguration
         * @interface IConfiguration
         * @property {SortType|null} [defaultSortOrder] Configuration defaultSortOrder
         */
    
        /**
         * Constructs a new Configuration.
         * @exports Configuration
         * @classdesc Represents a Configuration.
         * @implements IConfiguration
         * @constructor
         * @param {IConfiguration=} [properties] Properties to set
         */
        function Configuration(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Configuration defaultSortOrder.
         * @member {SortType} defaultSortOrder
         * @memberof Configuration
         * @instance
         */
        Configuration.prototype.defaultSortOrder = 0;
    
        /**
         * Creates a new Configuration instance using the specified properties.
         * @function create
         * @memberof Configuration
         * @static
         * @param {IConfiguration=} [properties] Properties to set
         * @returns {Configuration} Configuration instance
         */
        Configuration.create = function create(properties) {
            return new Configuration(properties);
        };
    
        /**
         * Encodes the specified Configuration message. Does not implicitly {@link Configuration.verify|verify} messages.
         * @function encode
         * @memberof Configuration
         * @static
         * @param {IConfiguration} message Configuration message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Configuration.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.defaultSortOrder != null && message.hasOwnProperty("defaultSortOrder"))
                writer.uint32(/* id 1, wireType 0 =*/8).int32(message.defaultSortOrder);
            return writer;
        };
    
        /**
         * Encodes the specified Configuration message, length delimited. Does not implicitly {@link Configuration.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Configuration
         * @static
         * @param {IConfiguration} message Configuration message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Configuration.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Configuration message from the specified reader or buffer.
         * @function decode
         * @memberof Configuration
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Configuration} Configuration
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Configuration.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Configuration();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.defaultSortOrder = reader.int32();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Configuration message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Configuration
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Configuration} Configuration
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Configuration.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Configuration message.
         * @function verify
         * @memberof Configuration
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Configuration.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.defaultSortOrder != null && message.hasOwnProperty("defaultSortOrder"))
                switch (message.defaultSortOrder) {
                default:
                    return "defaultSortOrder: enum value expected";
                case 0:
                case 1:
                case 2:
                    break;
                }
            return null;
        };
    
        /**
         * Creates a Configuration message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Configuration
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Configuration} Configuration
         */
        Configuration.fromObject = function fromObject(object) {
            if (object instanceof $root.Configuration)
                return object;
            var message = new $root.Configuration();
            switch (object.defaultSortOrder) {
            case "Name":
            case 0:
                message.defaultSortOrder = 0;
                break;
            case "Size":
            case 1:
                message.defaultSortOrder = 1;
                break;
            case "Date":
            case 2:
                message.defaultSortOrder = 2;
                break;
            }
            return message;
        };
    
        /**
         * Creates a plain object from a Configuration message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Configuration
         * @static
         * @param {Configuration} message Configuration
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Configuration.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.defaultSortOrder = options.enums === String ? "Name" : 0;
            if (message.defaultSortOrder != null && message.hasOwnProperty("defaultSortOrder"))
                object.defaultSortOrder = options.enums === String ? $root.SortType[message.defaultSortOrder] : message.defaultSortOrder;
            return object;
        };
    
        /**
         * Converts this Configuration to JSON.
         * @function toJSON
         * @memberof Configuration
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Configuration.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Configuration;
    })();
    
    $root.GetRequest = (function() {
    
        /**
         * Properties of a GetRequest.
         * @exports IGetRequest
         * @interface IGetRequest
         */
    
        /**
         * Constructs a new GetRequest.
         * @exports GetRequest
         * @classdesc Represents a GetRequest.
         * @implements IGetRequest
         * @constructor
         * @param {IGetRequest=} [properties] Properties to set
         */
        function GetRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new GetRequest instance using the specified properties.
         * @function create
         * @memberof GetRequest
         * @static
         * @param {IGetRequest=} [properties] Properties to set
         * @returns {GetRequest} GetRequest instance
         */
        GetRequest.create = function create(properties) {
            return new GetRequest(properties);
        };
    
        /**
         * Encodes the specified GetRequest message. Does not implicitly {@link GetRequest.verify|verify} messages.
         * @function encode
         * @memberof GetRequest
         * @static
         * @param {IGetRequest} message GetRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        GetRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified GetRequest message, length delimited. Does not implicitly {@link GetRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof GetRequest
         * @static
         * @param {IGetRequest} message GetRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        GetRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a GetRequest message from the specified reader or buffer.
         * @function decode
         * @memberof GetRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {GetRequest} GetRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        GetRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.GetRequest();
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
         * Decodes a GetRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof GetRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {GetRequest} GetRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        GetRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a GetRequest message.
         * @function verify
         * @memberof GetRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        GetRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a GetRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof GetRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {GetRequest} GetRequest
         */
        GetRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.GetRequest)
                return object;
            return new $root.GetRequest();
        };
    
        /**
         * Creates a plain object from a GetRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof GetRequest
         * @static
         * @param {GetRequest} message GetRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        GetRequest.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this GetRequest to JSON.
         * @function toJSON
         * @memberof GetRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        GetRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return GetRequest;
    })();
    
    $root.GetResponse = (function() {
    
        /**
         * Properties of a GetResponse.
         * @exports IGetResponse
         * @interface IGetResponse
         * @property {IConfiguration|null} [configuration] GetResponse configuration
         */
    
        /**
         * Constructs a new GetResponse.
         * @exports GetResponse
         * @classdesc Represents a GetResponse.
         * @implements IGetResponse
         * @constructor
         * @param {IGetResponse=} [properties] Properties to set
         */
        function GetResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * GetResponse configuration.
         * @member {IConfiguration|null|undefined} configuration
         * @memberof GetResponse
         * @instance
         */
        GetResponse.prototype.configuration = null;
    
        /**
         * Creates a new GetResponse instance using the specified properties.
         * @function create
         * @memberof GetResponse
         * @static
         * @param {IGetResponse=} [properties] Properties to set
         * @returns {GetResponse} GetResponse instance
         */
        GetResponse.create = function create(properties) {
            return new GetResponse(properties);
        };
    
        /**
         * Encodes the specified GetResponse message. Does not implicitly {@link GetResponse.verify|verify} messages.
         * @function encode
         * @memberof GetResponse
         * @static
         * @param {IGetResponse} message GetResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        GetResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.configuration != null && message.hasOwnProperty("configuration"))
                $root.Configuration.encode(message.configuration, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified GetResponse message, length delimited. Does not implicitly {@link GetResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof GetResponse
         * @static
         * @param {IGetResponse} message GetResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        GetResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a GetResponse message from the specified reader or buffer.
         * @function decode
         * @memberof GetResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {GetResponse} GetResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        GetResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.GetResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.configuration = $root.Configuration.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a GetResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof GetResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {GetResponse} GetResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        GetResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a GetResponse message.
         * @function verify
         * @memberof GetResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        GetResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.configuration != null && message.hasOwnProperty("configuration")) {
                var error = $root.Configuration.verify(message.configuration);
                if (error)
                    return "configuration." + error;
            }
            return null;
        };
    
        /**
         * Creates a GetResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof GetResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {GetResponse} GetResponse
         */
        GetResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.GetResponse)
                return object;
            var message = new $root.GetResponse();
            if (object.configuration != null) {
                if (typeof object.configuration !== "object")
                    throw TypeError(".GetResponse.configuration: object expected");
                message.configuration = $root.Configuration.fromObject(object.configuration);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a GetResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof GetResponse
         * @static
         * @param {GetResponse} message GetResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        GetResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.configuration = null;
            if (message.configuration != null && message.hasOwnProperty("configuration"))
                object.configuration = $root.Configuration.toObject(message.configuration, options);
            return object;
        };
    
        /**
         * Converts this GetResponse to JSON.
         * @function toJSON
         * @memberof GetResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        GetResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return GetResponse;
    })();
    
    $root.StoreRequest = (function() {
    
        /**
         * Properties of a StoreRequest.
         * @exports IStoreRequest
         * @interface IStoreRequest
         * @property {IConfiguration|null} [configuration] StoreRequest configuration
         */
    
        /**
         * Constructs a new StoreRequest.
         * @exports StoreRequest
         * @classdesc Represents a StoreRequest.
         * @implements IStoreRequest
         * @constructor
         * @param {IStoreRequest=} [properties] Properties to set
         */
        function StoreRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * StoreRequest configuration.
         * @member {IConfiguration|null|undefined} configuration
         * @memberof StoreRequest
         * @instance
         */
        StoreRequest.prototype.configuration = null;
    
        /**
         * Creates a new StoreRequest instance using the specified properties.
         * @function create
         * @memberof StoreRequest
         * @static
         * @param {IStoreRequest=} [properties] Properties to set
         * @returns {StoreRequest} StoreRequest instance
         */
        StoreRequest.create = function create(properties) {
            return new StoreRequest(properties);
        };
    
        /**
         * Encodes the specified StoreRequest message. Does not implicitly {@link StoreRequest.verify|verify} messages.
         * @function encode
         * @memberof StoreRequest
         * @static
         * @param {IStoreRequest} message StoreRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        StoreRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.configuration != null && message.hasOwnProperty("configuration"))
                $root.Configuration.encode(message.configuration, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified StoreRequest message, length delimited. Does not implicitly {@link StoreRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof StoreRequest
         * @static
         * @param {IStoreRequest} message StoreRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        StoreRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a StoreRequest message from the specified reader or buffer.
         * @function decode
         * @memberof StoreRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {StoreRequest} StoreRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        StoreRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.StoreRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.configuration = $root.Configuration.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a StoreRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof StoreRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {StoreRequest} StoreRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        StoreRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a StoreRequest message.
         * @function verify
         * @memberof StoreRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        StoreRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.configuration != null && message.hasOwnProperty("configuration")) {
                var error = $root.Configuration.verify(message.configuration);
                if (error)
                    return "configuration." + error;
            }
            return null;
        };
    
        /**
         * Creates a StoreRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof StoreRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {StoreRequest} StoreRequest
         */
        StoreRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.StoreRequest)
                return object;
            var message = new $root.StoreRequest();
            if (object.configuration != null) {
                if (typeof object.configuration !== "object")
                    throw TypeError(".StoreRequest.configuration: object expected");
                message.configuration = $root.Configuration.fromObject(object.configuration);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a StoreRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof StoreRequest
         * @static
         * @param {StoreRequest} message StoreRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        StoreRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.configuration = null;
            if (message.configuration != null && message.hasOwnProperty("configuration"))
                object.configuration = $root.Configuration.toObject(message.configuration, options);
            return object;
        };
    
        /**
         * Converts this StoreRequest to JSON.
         * @function toJSON
         * @memberof StoreRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        StoreRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return StoreRequest;
    })();
    
    $root.StoreResponse = (function() {
    
        /**
         * Properties of a StoreResponse.
         * @exports IStoreResponse
         * @interface IStoreResponse
         */
    
        /**
         * Constructs a new StoreResponse.
         * @exports StoreResponse
         * @classdesc Represents a StoreResponse.
         * @implements IStoreResponse
         * @constructor
         * @param {IStoreResponse=} [properties] Properties to set
         */
        function StoreResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new StoreResponse instance using the specified properties.
         * @function create
         * @memberof StoreResponse
         * @static
         * @param {IStoreResponse=} [properties] Properties to set
         * @returns {StoreResponse} StoreResponse instance
         */
        StoreResponse.create = function create(properties) {
            return new StoreResponse(properties);
        };
    
        /**
         * Encodes the specified StoreResponse message. Does not implicitly {@link StoreResponse.verify|verify} messages.
         * @function encode
         * @memberof StoreResponse
         * @static
         * @param {IStoreResponse} message StoreResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        StoreResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified StoreResponse message, length delimited. Does not implicitly {@link StoreResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof StoreResponse
         * @static
         * @param {IStoreResponse} message StoreResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        StoreResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a StoreResponse message from the specified reader or buffer.
         * @function decode
         * @memberof StoreResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {StoreResponse} StoreResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        StoreResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.StoreResponse();
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
         * Decodes a StoreResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof StoreResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {StoreResponse} StoreResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        StoreResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a StoreResponse message.
         * @function verify
         * @memberof StoreResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        StoreResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a StoreResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof StoreResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {StoreResponse} StoreResponse
         */
        StoreResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.StoreResponse)
                return object;
            return new $root.StoreResponse();
        };
    
        /**
         * Creates a plain object from a StoreResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof StoreResponse
         * @static
         * @param {StoreResponse} message StoreResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        StoreResponse.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this StoreResponse to JSON.
         * @function toJSON
         * @memberof StoreResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        StoreResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return StoreResponse;
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
