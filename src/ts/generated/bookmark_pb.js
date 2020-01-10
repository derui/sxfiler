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
    
    $root.Bookmark = (function() {
    
        /**
         * Properties of a Bookmark.
         * @exports IBookmark
         * @interface IBookmark
         * @property {string|null} [id] Bookmark id
         * @property {string|null} [path] Bookmark path
         * @property {number|null} [order] Bookmark order
         */
    
        /**
         * Constructs a new Bookmark.
         * @exports Bookmark
         * @classdesc Represents a Bookmark.
         * @implements IBookmark
         * @constructor
         * @param {IBookmark=} [properties] Properties to set
         */
        function Bookmark(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Bookmark id.
         * @member {string} id
         * @memberof Bookmark
         * @instance
         */
        Bookmark.prototype.id = "";
    
        /**
         * Bookmark path.
         * @member {string} path
         * @memberof Bookmark
         * @instance
         */
        Bookmark.prototype.path = "";
    
        /**
         * Bookmark order.
         * @member {number} order
         * @memberof Bookmark
         * @instance
         */
        Bookmark.prototype.order = 0;
    
        /**
         * Creates a new Bookmark instance using the specified properties.
         * @function create
         * @memberof Bookmark
         * @static
         * @param {IBookmark=} [properties] Properties to set
         * @returns {Bookmark} Bookmark instance
         */
        Bookmark.create = function create(properties) {
            return new Bookmark(properties);
        };
    
        /**
         * Encodes the specified Bookmark message. Does not implicitly {@link Bookmark.verify|verify} messages.
         * @function encode
         * @memberof Bookmark
         * @static
         * @param {IBookmark} message Bookmark message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Bookmark.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.id != null && message.hasOwnProperty("id"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.id);
            if (message.path != null && message.hasOwnProperty("path"))
                writer.uint32(/* id 2, wireType 2 =*/18).string(message.path);
            if (message.order != null && message.hasOwnProperty("order"))
                writer.uint32(/* id 3, wireType 0 =*/24).int32(message.order);
            return writer;
        };
    
        /**
         * Encodes the specified Bookmark message, length delimited. Does not implicitly {@link Bookmark.verify|verify} messages.
         * @function encodeDelimited
         * @memberof Bookmark
         * @static
         * @param {IBookmark} message Bookmark message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Bookmark.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a Bookmark message from the specified reader or buffer.
         * @function decode
         * @memberof Bookmark
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {Bookmark} Bookmark
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Bookmark.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.Bookmark();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.id = reader.string();
                    break;
                case 2:
                    message.path = reader.string();
                    break;
                case 3:
                    message.order = reader.int32();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a Bookmark message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof Bookmark
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {Bookmark} Bookmark
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Bookmark.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a Bookmark message.
         * @function verify
         * @memberof Bookmark
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Bookmark.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.id != null && message.hasOwnProperty("id"))
                if (!$util.isString(message.id))
                    return "id: string expected";
            if (message.path != null && message.hasOwnProperty("path"))
                if (!$util.isString(message.path))
                    return "path: string expected";
            if (message.order != null && message.hasOwnProperty("order"))
                if (!$util.isInteger(message.order))
                    return "order: integer expected";
            return null;
        };
    
        /**
         * Creates a Bookmark message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof Bookmark
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {Bookmark} Bookmark
         */
        Bookmark.fromObject = function fromObject(object) {
            if (object instanceof $root.Bookmark)
                return object;
            var message = new $root.Bookmark();
            if (object.id != null)
                message.id = String(object.id);
            if (object.path != null)
                message.path = String(object.path);
            if (object.order != null)
                message.order = object.order | 0;
            return message;
        };
    
        /**
         * Creates a plain object from a Bookmark message. Also converts values to other types if specified.
         * @function toObject
         * @memberof Bookmark
         * @static
         * @param {Bookmark} message Bookmark
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Bookmark.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults) {
                object.id = "";
                object.path = "";
                object.order = 0;
            }
            if (message.id != null && message.hasOwnProperty("id"))
                object.id = message.id;
            if (message.path != null && message.hasOwnProperty("path"))
                object.path = message.path;
            if (message.order != null && message.hasOwnProperty("order"))
                object.order = message.order;
            return object;
        };
    
        /**
         * Converts this Bookmark to JSON.
         * @function toJSON
         * @memberof Bookmark
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Bookmark.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return Bookmark;
    })();
    
    $root.ListAllRequest = (function() {
    
        /**
         * Properties of a ListAllRequest.
         * @exports IListAllRequest
         * @interface IListAllRequest
         */
    
        /**
         * Constructs a new ListAllRequest.
         * @exports ListAllRequest
         * @classdesc Represents a ListAllRequest.
         * @implements IListAllRequest
         * @constructor
         * @param {IListAllRequest=} [properties] Properties to set
         */
        function ListAllRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * Creates a new ListAllRequest instance using the specified properties.
         * @function create
         * @memberof ListAllRequest
         * @static
         * @param {IListAllRequest=} [properties] Properties to set
         * @returns {ListAllRequest} ListAllRequest instance
         */
        ListAllRequest.create = function create(properties) {
            return new ListAllRequest(properties);
        };
    
        /**
         * Encodes the specified ListAllRequest message. Does not implicitly {@link ListAllRequest.verify|verify} messages.
         * @function encode
         * @memberof ListAllRequest
         * @static
         * @param {IListAllRequest} message ListAllRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ListAllRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            return writer;
        };
    
        /**
         * Encodes the specified ListAllRequest message, length delimited. Does not implicitly {@link ListAllRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof ListAllRequest
         * @static
         * @param {IListAllRequest} message ListAllRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ListAllRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a ListAllRequest message from the specified reader or buffer.
         * @function decode
         * @memberof ListAllRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {ListAllRequest} ListAllRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ListAllRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.ListAllRequest();
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
         * Decodes a ListAllRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof ListAllRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {ListAllRequest} ListAllRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ListAllRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a ListAllRequest message.
         * @function verify
         * @memberof ListAllRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        ListAllRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            return null;
        };
    
        /**
         * Creates a ListAllRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof ListAllRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {ListAllRequest} ListAllRequest
         */
        ListAllRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.ListAllRequest)
                return object;
            return new $root.ListAllRequest();
        };
    
        /**
         * Creates a plain object from a ListAllRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof ListAllRequest
         * @static
         * @param {ListAllRequest} message ListAllRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        ListAllRequest.toObject = function toObject() {
            return {};
        };
    
        /**
         * Converts this ListAllRequest to JSON.
         * @function toJSON
         * @memberof ListAllRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        ListAllRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return ListAllRequest;
    })();
    
    $root.ListAllResponse = (function() {
    
        /**
         * Properties of a ListAllResponse.
         * @exports IListAllResponse
         * @interface IListAllResponse
         * @property {Array.<IBookmark>|null} [bookmarks] ListAllResponse bookmarks
         */
    
        /**
         * Constructs a new ListAllResponse.
         * @exports ListAllResponse
         * @classdesc Represents a ListAllResponse.
         * @implements IListAllResponse
         * @constructor
         * @param {IListAllResponse=} [properties] Properties to set
         */
        function ListAllResponse(properties) {
            this.bookmarks = [];
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * ListAllResponse bookmarks.
         * @member {Array.<IBookmark>} bookmarks
         * @memberof ListAllResponse
         * @instance
         */
        ListAllResponse.prototype.bookmarks = $util.emptyArray;
    
        /**
         * Creates a new ListAllResponse instance using the specified properties.
         * @function create
         * @memberof ListAllResponse
         * @static
         * @param {IListAllResponse=} [properties] Properties to set
         * @returns {ListAllResponse} ListAllResponse instance
         */
        ListAllResponse.create = function create(properties) {
            return new ListAllResponse(properties);
        };
    
        /**
         * Encodes the specified ListAllResponse message. Does not implicitly {@link ListAllResponse.verify|verify} messages.
         * @function encode
         * @memberof ListAllResponse
         * @static
         * @param {IListAllResponse} message ListAllResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ListAllResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.bookmarks != null && message.bookmarks.length)
                for (var i = 0; i < message.bookmarks.length; ++i)
                    $root.Bookmark.encode(message.bookmarks[i], writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified ListAllResponse message, length delimited. Does not implicitly {@link ListAllResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof ListAllResponse
         * @static
         * @param {IListAllResponse} message ListAllResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        ListAllResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a ListAllResponse message from the specified reader or buffer.
         * @function decode
         * @memberof ListAllResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {ListAllResponse} ListAllResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ListAllResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.ListAllResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    if (!(message.bookmarks && message.bookmarks.length))
                        message.bookmarks = [];
                    message.bookmarks.push($root.Bookmark.decode(reader, reader.uint32()));
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a ListAllResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof ListAllResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {ListAllResponse} ListAllResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        ListAllResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a ListAllResponse message.
         * @function verify
         * @memberof ListAllResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        ListAllResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.bookmarks != null && message.hasOwnProperty("bookmarks")) {
                if (!Array.isArray(message.bookmarks))
                    return "bookmarks: array expected";
                for (var i = 0; i < message.bookmarks.length; ++i) {
                    var error = $root.Bookmark.verify(message.bookmarks[i]);
                    if (error)
                        return "bookmarks." + error;
                }
            }
            return null;
        };
    
        /**
         * Creates a ListAllResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof ListAllResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {ListAllResponse} ListAllResponse
         */
        ListAllResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.ListAllResponse)
                return object;
            var message = new $root.ListAllResponse();
            if (object.bookmarks) {
                if (!Array.isArray(object.bookmarks))
                    throw TypeError(".ListAllResponse.bookmarks: array expected");
                message.bookmarks = [];
                for (var i = 0; i < object.bookmarks.length; ++i) {
                    if (typeof object.bookmarks[i] !== "object")
                        throw TypeError(".ListAllResponse.bookmarks: object expected");
                    message.bookmarks[i] = $root.Bookmark.fromObject(object.bookmarks[i]);
                }
            }
            return message;
        };
    
        /**
         * Creates a plain object from a ListAllResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof ListAllResponse
         * @static
         * @param {ListAllResponse} message ListAllResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        ListAllResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.arrays || options.defaults)
                object.bookmarks = [];
            if (message.bookmarks && message.bookmarks.length) {
                object.bookmarks = [];
                for (var j = 0; j < message.bookmarks.length; ++j)
                    object.bookmarks[j] = $root.Bookmark.toObject(message.bookmarks[j], options);
            }
            return object;
        };
    
        /**
         * Converts this ListAllResponse to JSON.
         * @function toJSON
         * @memberof ListAllResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        ListAllResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return ListAllResponse;
    })();
    
    $root.RegisterRequest = (function() {
    
        /**
         * Properties of a RegisterRequest.
         * @exports IRegisterRequest
         * @interface IRegisterRequest
         * @property {string|null} [path] RegisterRequest path
         */
    
        /**
         * Constructs a new RegisterRequest.
         * @exports RegisterRequest
         * @classdesc Represents a RegisterRequest.
         * @implements IRegisterRequest
         * @constructor
         * @param {IRegisterRequest=} [properties] Properties to set
         */
        function RegisterRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * RegisterRequest path.
         * @member {string} path
         * @memberof RegisterRequest
         * @instance
         */
        RegisterRequest.prototype.path = "";
    
        /**
         * Creates a new RegisterRequest instance using the specified properties.
         * @function create
         * @memberof RegisterRequest
         * @static
         * @param {IRegisterRequest=} [properties] Properties to set
         * @returns {RegisterRequest} RegisterRequest instance
         */
        RegisterRequest.create = function create(properties) {
            return new RegisterRequest(properties);
        };
    
        /**
         * Encodes the specified RegisterRequest message. Does not implicitly {@link RegisterRequest.verify|verify} messages.
         * @function encode
         * @memberof RegisterRequest
         * @static
         * @param {IRegisterRequest} message RegisterRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        RegisterRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.path != null && message.hasOwnProperty("path"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.path);
            return writer;
        };
    
        /**
         * Encodes the specified RegisterRequest message, length delimited. Does not implicitly {@link RegisterRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof RegisterRequest
         * @static
         * @param {IRegisterRequest} message RegisterRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        RegisterRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a RegisterRequest message from the specified reader or buffer.
         * @function decode
         * @memberof RegisterRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {RegisterRequest} RegisterRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        RegisterRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.RegisterRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.path = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a RegisterRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof RegisterRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {RegisterRequest} RegisterRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        RegisterRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a RegisterRequest message.
         * @function verify
         * @memberof RegisterRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        RegisterRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.path != null && message.hasOwnProperty("path"))
                if (!$util.isString(message.path))
                    return "path: string expected";
            return null;
        };
    
        /**
         * Creates a RegisterRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof RegisterRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {RegisterRequest} RegisterRequest
         */
        RegisterRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.RegisterRequest)
                return object;
            var message = new $root.RegisterRequest();
            if (object.path != null)
                message.path = String(object.path);
            return message;
        };
    
        /**
         * Creates a plain object from a RegisterRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof RegisterRequest
         * @static
         * @param {RegisterRequest} message RegisterRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        RegisterRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.path = "";
            if (message.path != null && message.hasOwnProperty("path"))
                object.path = message.path;
            return object;
        };
    
        /**
         * Converts this RegisterRequest to JSON.
         * @function toJSON
         * @memberof RegisterRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        RegisterRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return RegisterRequest;
    })();
    
    $root.RegisterResponse = (function() {
    
        /**
         * Properties of a RegisterResponse.
         * @exports IRegisterResponse
         * @interface IRegisterResponse
         * @property {IBookmark|null} [bookmark] RegisterResponse bookmark
         */
    
        /**
         * Constructs a new RegisterResponse.
         * @exports RegisterResponse
         * @classdesc Represents a RegisterResponse.
         * @implements IRegisterResponse
         * @constructor
         * @param {IRegisterResponse=} [properties] Properties to set
         */
        function RegisterResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * RegisterResponse bookmark.
         * @member {IBookmark|null|undefined} bookmark
         * @memberof RegisterResponse
         * @instance
         */
        RegisterResponse.prototype.bookmark = null;
    
        /**
         * Creates a new RegisterResponse instance using the specified properties.
         * @function create
         * @memberof RegisterResponse
         * @static
         * @param {IRegisterResponse=} [properties] Properties to set
         * @returns {RegisterResponse} RegisterResponse instance
         */
        RegisterResponse.create = function create(properties) {
            return new RegisterResponse(properties);
        };
    
        /**
         * Encodes the specified RegisterResponse message. Does not implicitly {@link RegisterResponse.verify|verify} messages.
         * @function encode
         * @memberof RegisterResponse
         * @static
         * @param {IRegisterResponse} message RegisterResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        RegisterResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.bookmark != null && message.hasOwnProperty("bookmark"))
                $root.Bookmark.encode(message.bookmark, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified RegisterResponse message, length delimited. Does not implicitly {@link RegisterResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof RegisterResponse
         * @static
         * @param {IRegisterResponse} message RegisterResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        RegisterResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a RegisterResponse message from the specified reader or buffer.
         * @function decode
         * @memberof RegisterResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {RegisterResponse} RegisterResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        RegisterResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.RegisterResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.bookmark = $root.Bookmark.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a RegisterResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof RegisterResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {RegisterResponse} RegisterResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        RegisterResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a RegisterResponse message.
         * @function verify
         * @memberof RegisterResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        RegisterResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.bookmark != null && message.hasOwnProperty("bookmark")) {
                var error = $root.Bookmark.verify(message.bookmark);
                if (error)
                    return "bookmark." + error;
            }
            return null;
        };
    
        /**
         * Creates a RegisterResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof RegisterResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {RegisterResponse} RegisterResponse
         */
        RegisterResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.RegisterResponse)
                return object;
            var message = new $root.RegisterResponse();
            if (object.bookmark != null) {
                if (typeof object.bookmark !== "object")
                    throw TypeError(".RegisterResponse.bookmark: object expected");
                message.bookmark = $root.Bookmark.fromObject(object.bookmark);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a RegisterResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof RegisterResponse
         * @static
         * @param {RegisterResponse} message RegisterResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        RegisterResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.bookmark = null;
            if (message.bookmark != null && message.hasOwnProperty("bookmark"))
                object.bookmark = $root.Bookmark.toObject(message.bookmark, options);
            return object;
        };
    
        /**
         * Converts this RegisterResponse to JSON.
         * @function toJSON
         * @memberof RegisterResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        RegisterResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return RegisterResponse;
    })();
    
    $root.DeleteRequest = (function() {
    
        /**
         * Properties of a DeleteRequest.
         * @exports IDeleteRequest
         * @interface IDeleteRequest
         * @property {string|null} [id] DeleteRequest id
         */
    
        /**
         * Constructs a new DeleteRequest.
         * @exports DeleteRequest
         * @classdesc Represents a DeleteRequest.
         * @implements IDeleteRequest
         * @constructor
         * @param {IDeleteRequest=} [properties] Properties to set
         */
        function DeleteRequest(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * DeleteRequest id.
         * @member {string} id
         * @memberof DeleteRequest
         * @instance
         */
        DeleteRequest.prototype.id = "";
    
        /**
         * Creates a new DeleteRequest instance using the specified properties.
         * @function create
         * @memberof DeleteRequest
         * @static
         * @param {IDeleteRequest=} [properties] Properties to set
         * @returns {DeleteRequest} DeleteRequest instance
         */
        DeleteRequest.create = function create(properties) {
            return new DeleteRequest(properties);
        };
    
        /**
         * Encodes the specified DeleteRequest message. Does not implicitly {@link DeleteRequest.verify|verify} messages.
         * @function encode
         * @memberof DeleteRequest
         * @static
         * @param {IDeleteRequest} message DeleteRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        DeleteRequest.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.id != null && message.hasOwnProperty("id"))
                writer.uint32(/* id 1, wireType 2 =*/10).string(message.id);
            return writer;
        };
    
        /**
         * Encodes the specified DeleteRequest message, length delimited. Does not implicitly {@link DeleteRequest.verify|verify} messages.
         * @function encodeDelimited
         * @memberof DeleteRequest
         * @static
         * @param {IDeleteRequest} message DeleteRequest message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        DeleteRequest.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a DeleteRequest message from the specified reader or buffer.
         * @function decode
         * @memberof DeleteRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {DeleteRequest} DeleteRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        DeleteRequest.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.DeleteRequest();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.id = reader.string();
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a DeleteRequest message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof DeleteRequest
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {DeleteRequest} DeleteRequest
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        DeleteRequest.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a DeleteRequest message.
         * @function verify
         * @memberof DeleteRequest
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        DeleteRequest.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.id != null && message.hasOwnProperty("id"))
                if (!$util.isString(message.id))
                    return "id: string expected";
            return null;
        };
    
        /**
         * Creates a DeleteRequest message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof DeleteRequest
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {DeleteRequest} DeleteRequest
         */
        DeleteRequest.fromObject = function fromObject(object) {
            if (object instanceof $root.DeleteRequest)
                return object;
            var message = new $root.DeleteRequest();
            if (object.id != null)
                message.id = String(object.id);
            return message;
        };
    
        /**
         * Creates a plain object from a DeleteRequest message. Also converts values to other types if specified.
         * @function toObject
         * @memberof DeleteRequest
         * @static
         * @param {DeleteRequest} message DeleteRequest
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        DeleteRequest.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.id = "";
            if (message.id != null && message.hasOwnProperty("id"))
                object.id = message.id;
            return object;
        };
    
        /**
         * Converts this DeleteRequest to JSON.
         * @function toJSON
         * @memberof DeleteRequest
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        DeleteRequest.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return DeleteRequest;
    })();
    
    $root.DeleteResponse = (function() {
    
        /**
         * Properties of a DeleteResponse.
         * @exports IDeleteResponse
         * @interface IDeleteResponse
         * @property {IBookmark|null} [deletedBookmark] DeleteResponse deletedBookmark
         */
    
        /**
         * Constructs a new DeleteResponse.
         * @exports DeleteResponse
         * @classdesc Represents a DeleteResponse.
         * @implements IDeleteResponse
         * @constructor
         * @param {IDeleteResponse=} [properties] Properties to set
         */
        function DeleteResponse(properties) {
            if (properties)
                for (var keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }
    
        /**
         * DeleteResponse deletedBookmark.
         * @member {IBookmark|null|undefined} deletedBookmark
         * @memberof DeleteResponse
         * @instance
         */
        DeleteResponse.prototype.deletedBookmark = null;
    
        /**
         * Creates a new DeleteResponse instance using the specified properties.
         * @function create
         * @memberof DeleteResponse
         * @static
         * @param {IDeleteResponse=} [properties] Properties to set
         * @returns {DeleteResponse} DeleteResponse instance
         */
        DeleteResponse.create = function create(properties) {
            return new DeleteResponse(properties);
        };
    
        /**
         * Encodes the specified DeleteResponse message. Does not implicitly {@link DeleteResponse.verify|verify} messages.
         * @function encode
         * @memberof DeleteResponse
         * @static
         * @param {IDeleteResponse} message DeleteResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        DeleteResponse.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.deletedBookmark != null && message.hasOwnProperty("deletedBookmark"))
                $root.Bookmark.encode(message.deletedBookmark, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };
    
        /**
         * Encodes the specified DeleteResponse message, length delimited. Does not implicitly {@link DeleteResponse.verify|verify} messages.
         * @function encodeDelimited
         * @memberof DeleteResponse
         * @static
         * @param {IDeleteResponse} message DeleteResponse message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        DeleteResponse.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };
    
        /**
         * Decodes a DeleteResponse message from the specified reader or buffer.
         * @function decode
         * @memberof DeleteResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {DeleteResponse} DeleteResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        DeleteResponse.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            var end = length === undefined ? reader.len : reader.pos + length, message = new $root.DeleteResponse();
            while (reader.pos < end) {
                var tag = reader.uint32();
                switch (tag >>> 3) {
                case 1:
                    message.deletedBookmark = $root.Bookmark.decode(reader, reader.uint32());
                    break;
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };
    
        /**
         * Decodes a DeleteResponse message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof DeleteResponse
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {DeleteResponse} DeleteResponse
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        DeleteResponse.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };
    
        /**
         * Verifies a DeleteResponse message.
         * @function verify
         * @memberof DeleteResponse
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        DeleteResponse.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.deletedBookmark != null && message.hasOwnProperty("deletedBookmark")) {
                var error = $root.Bookmark.verify(message.deletedBookmark);
                if (error)
                    return "deletedBookmark." + error;
            }
            return null;
        };
    
        /**
         * Creates a DeleteResponse message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof DeleteResponse
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {DeleteResponse} DeleteResponse
         */
        DeleteResponse.fromObject = function fromObject(object) {
            if (object instanceof $root.DeleteResponse)
                return object;
            var message = new $root.DeleteResponse();
            if (object.deletedBookmark != null) {
                if (typeof object.deletedBookmark !== "object")
                    throw TypeError(".DeleteResponse.deletedBookmark: object expected");
                message.deletedBookmark = $root.Bookmark.fromObject(object.deletedBookmark);
            }
            return message;
        };
    
        /**
         * Creates a plain object from a DeleteResponse message. Also converts values to other types if specified.
         * @function toObject
         * @memberof DeleteResponse
         * @static
         * @param {DeleteResponse} message DeleteResponse
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        DeleteResponse.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            var object = {};
            if (options.defaults)
                object.deletedBookmark = null;
            if (message.deletedBookmark != null && message.hasOwnProperty("deletedBookmark"))
                object.deletedBookmark = $root.Bookmark.toObject(message.deletedBookmark, options);
            return object;
        };
    
        /**
         * Converts this DeleteResponse to JSON.
         * @function toJSON
         * @memberof DeleteResponse
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        DeleteResponse.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };
    
        return DeleteResponse;
    })();

    return $root;
});
