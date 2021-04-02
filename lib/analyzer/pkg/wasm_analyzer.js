import * as wasm from './wasm_analyzer_bg.wasm';

/**
*/
export function start() {
    wasm.start();
}

let WASM_VECTOR_LEN = 0;

const lTextEncoder = typeof TextEncoder === 'undefined' ? require('util').TextEncoder : TextEncoder;

let cachedTextEncoder = new lTextEncoder('utf-8');

const encodeString = (typeof cachedTextEncoder.encodeInto === 'function'
    ? function (arg, view) {
    return cachedTextEncoder.encodeInto(arg, view);
}
    : function (arg, view) {
    const buf = cachedTextEncoder.encode(arg);
    view.set(buf);
    return {
        read: arg.length,
        written: buf.length
    };
});

let cachegetUint8Memory = null;
function getUint8Memory() {
    if (cachegetUint8Memory === null || cachegetUint8Memory.buffer !== wasm.memory.buffer) {
        cachegetUint8Memory = new Uint8Array(wasm.memory.buffer);
    }
    return cachegetUint8Memory;
}

function passStringToWasm(arg) {

    let len = arg.length;
    let ptr = wasm.__wbindgen_malloc(len);

    const mem = getUint8Memory();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }

    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = wasm.__wbindgen_realloc(ptr, len, len = offset + arg.length * 3);
        const view = getUint8Memory().subarray(ptr + offset, ptr + len);
        const ret = encodeString(arg, view);

        offset += ret.written;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

const heap = new Array(32);

heap.fill(undefined);

heap.push(undefined, null, true, false);

function getObject(idx) { return heap[idx]; }

let heap_next = heap.length;

function dropObject(idx) {
    if (idx < 36) return;
    heap[idx] = heap_next;
    heap_next = idx;
}

function takeObject(idx) {
    const ret = getObject(idx);
    dropObject(idx);
    return ret;
}

const lTextDecoder = typeof TextDecoder === 'undefined' ? require('util').TextDecoder : TextDecoder;

let cachedTextDecoder = new lTextDecoder('utf-8', { ignoreBOM: true, fatal: true });

cachedTextDecoder.decode();

function getStringFromWasm(ptr, len) {
    return cachedTextDecoder.decode(getUint8Memory().subarray(ptr, ptr + len));
}

function addHeapObject(obj) {
    if (heap_next === heap.length) heap.push(heap.length + 1);
    const idx = heap_next;
    heap_next = heap[idx];

    heap[idx] = obj;
    return idx;
}

let cachegetInt32Memory = null;
function getInt32Memory() {
    if (cachegetInt32Memory === null || cachegetInt32Memory.buffer !== wasm.memory.buffer) {
        cachegetInt32Memory = new Int32Array(wasm.memory.buffer);
    }
    return cachegetInt32Memory;
}

function debugString(val) {
    // primitive types
    const type = typeof val;
    if (type == 'number' || type == 'boolean' || val == null) {
        return  `${val}`;
    }
    if (type == 'string') {
        return `"${val}"`;
    }
    if (type == 'symbol') {
        const description = val.description;
        if (description == null) {
            return 'Symbol';
        } else {
            return `Symbol(${description})`;
        }
    }
    if (type == 'function') {
        const name = val.name;
        if (typeof name == 'string' && name.length > 0) {
            return `Function(${name})`;
        } else {
            return 'Function';
        }
    }
    // objects
    if (Array.isArray(val)) {
        const length = val.length;
        let debug = '[';
        if (length > 0) {
            debug += debugString(val[0]);
        }
        for(let i = 1; i < length; i++) {
            debug += ', ' + debugString(val[i]);
        }
        debug += ']';
        return debug;
    }
    // Test for built-in
    const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
    let className;
    if (builtInMatches.length > 1) {
        className = builtInMatches[1];
    } else {
        // Failed to match the standard '[object ClassName]'
        return toString.call(val);
    }
    if (className == 'Object') {
        // we're a user defined class or Object
        // JSON.stringify avoids problems with cycles, and is generally much
        // easier than looping through ownProperties of `val`.
        try {
            return 'Object(' + JSON.stringify(val) + ')';
        } catch (_) {
            return 'Object';
        }
    }
    // errors
    if (val instanceof Error) {
        return `${val.name}: ${val.message}\n${val.stack}`;
    }
    // TODO we could test for more things here, like `Set`s and `Map`s.
    return className;
}
/**
*/
export class WorldState {

    static __wrap(ptr) {
        const obj = Object.create(WorldState.prototype);
        obj.ptr = ptr;

        return obj;
    }

    free() {
        const ptr = this.ptr;
        this.ptr = 0;

        wasm.__wbg_worldstate_free(ptr);
    }
    /**
    * @returns {WorldState}
    */
    constructor() {
        const ret = wasm.worldstate_new();
        return WorldState.__wrap(ret);
    }
    /**
    * @param {string} code
    * @returns {any}
    */
    update(code) {
        const ret = wasm.worldstate_update(this.ptr, passStringToWasm(code), WASM_VECTOR_LEN);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    completions(line_number, column) {
        const ret = wasm.worldstate_completions(this.ptr, line_number, column);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    hover(line_number, column) {
        const ret = wasm.worldstate_hover(this.ptr, line_number, column);
        return takeObject(ret);
    }
    /**
    * @returns {any}
    */
    code_lenses() {
        const ret = wasm.worldstate_code_lenses(this.ptr);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @param {boolean} include_declaration
    * @returns {any}
    */
    references(line_number, column, include_declaration) {
        const ret = wasm.worldstate_references(this.ptr, line_number, column, include_declaration);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    prepare_rename(line_number, column) {
        const ret = wasm.worldstate_prepare_rename(this.ptr, line_number, column);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @param {string} new_name
    * @returns {any}
    */
    rename(line_number, column, new_name) {
        const ret = wasm.worldstate_rename(this.ptr, line_number, column, passStringToWasm(new_name), WASM_VECTOR_LEN);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    signature_help(line_number, column) {
        const ret = wasm.worldstate_signature_help(this.ptr, line_number, column);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    definition(line_number, column) {
        const ret = wasm.worldstate_definition(this.ptr, line_number, column);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    type_definition(line_number, column) {
        const ret = wasm.worldstate_type_definition(this.ptr, line_number, column);
        return takeObject(ret);
    }
    /**
    * @returns {any}
    */
    document_symbols() {
        const ret = wasm.worldstate_document_symbols(this.ptr);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @param {string} ch
    * @returns {any}
    */
    type_formatting(line_number, column, ch) {
        const ret = wasm.worldstate_type_formatting(this.ptr, line_number, column, ch.codePointAt(0));
        return takeObject(ret);
    }
    /**
    * @returns {any}
    */
    folding_ranges() {
        const ret = wasm.worldstate_folding_ranges(this.ptr);
        return takeObject(ret);
    }
    /**
    * @param {number} line_number
    * @param {number} column
    * @returns {any}
    */
    goto_implementation(line_number, column) {
        const ret = wasm.worldstate_goto_implementation(this.ptr, line_number, column);
        return takeObject(ret);
    }
}

export const __wbindgen_object_drop_ref = function(arg0) {
    takeObject(arg0);
};

export const __wbindgen_string_new = function(arg0, arg1) {
    const ret = getStringFromWasm(arg0, arg1);
    return addHeapObject(ret);
};

export const __wbindgen_object_clone_ref = function(arg0) {
    const ret = getObject(arg0);
    return addHeapObject(ret);
};

export const __wbg_new_68adb0d58759a4ed = function() {
    const ret = new Object();
    return addHeapObject(ret);
};

export const __wbindgen_number_new = function(arg0) {
    const ret = arg0;
    return addHeapObject(ret);
};

export const __wbg_set_2e79e744454afade = function(arg0, arg1, arg2) {
    getObject(arg0)[takeObject(arg1)] = takeObject(arg2);
};

export const __wbg_new_8caf63c832ef170c = function() {
    const ret = new Array();
    return addHeapObject(ret);
};

export const __wbg_push_3ed2be796ba59b8b = function(arg0, arg1) {
    const ret = getObject(arg0).push(getObject(arg1));
    return ret;
};

export const __wbg_new_4aa8e1d51fa62e60 = function(arg0, arg1) {
    const ret = new Error(getStringFromWasm(arg0, arg1));
    return addHeapObject(ret);
};

export const __wbindgen_debug_string = function(arg0, arg1) {
    const ret = debugString(getObject(arg1));
    const ret0 = passStringToWasm(ret);
    const ret1 = WASM_VECTOR_LEN;
    getInt32Memory()[arg0 / 4 + 0] = ret0;
    getInt32Memory()[arg0 / 4 + 1] = ret1;
};

export const __wbindgen_throw = function(arg0, arg1) {
    throw new Error(getStringFromWasm(arg0, arg1));
};

wasm.__wbindgen_start();

