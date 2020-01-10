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
