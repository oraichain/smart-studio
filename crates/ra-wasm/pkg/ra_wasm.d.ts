/* tslint:disable */
/* eslint-disable */
/**
*/
export function start(): void;
/**
* @param {number} num_threads
* @returns {Promise<any>}
*/
export function initThreadPool(num_threads: number): Promise<any>;
/**
* @param {number} receiver
*/
export function wbg_rayon_start_worker(receiver: number): void;
/**
*/
export class WorldState {
  free(): void;
/**
*/
  constructor();
/**
* @param {Uint8Array} json
*/
  load(json: Uint8Array): void;
/**
* @param {string} rust_std
* @param {string} rust_core
* @param {string} rust_alloc
* @param {string} rust_cosmwasm_derive
* @param {string} rust_cosmwasm_schema_derive
* @param {string} rust_cosmwasm_schema
* @param {string} rust_cosmwasm_std
* @param {string} rust_cosmwasm_crypto
* @param {string} rust_cosmwasm_storage
* @param {string} rust_thiserror
* @param {string} rust_thiserror_impl
* @param {string} rust_proc_macro2
*/
  init(rust_std: string, rust_core: string, rust_alloc: string, rust_cosmwasm_derive: string, rust_cosmwasm_schema_derive: string, rust_cosmwasm_schema: string, rust_cosmwasm_std: string, rust_cosmwasm_crypto: string, rust_cosmwasm_storage: string, rust_thiserror: string, rust_thiserror_impl: string, rust_proc_macro2: string): void;
/**
* @param {number} file_ind
* @param {string} code
* @param {boolean | undefined} with_highlight
* @returns {any}
*/
  update(file_ind: number, code: string, with_highlight?: boolean): any;
/**
* @param {number} file_ind
* @returns {any}
*/
  inlay_hints(file_ind: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  completions(file_ind: number, line_number: number, column: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  hover(file_ind: number, line_number: number, column: number): any;
/**
* @param {number} file_ind
* @returns {any}
*/
  code_lenses(file_ind: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @param {boolean} include_declaration
* @returns {any}
*/
  references(file_ind: number, line_number: number, column: number, include_declaration: boolean): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  prepare_rename(file_ind: number, line_number: number, column: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @param {string} new_name
* @returns {any}
*/
  rename(file_ind: number, line_number: number, column: number, new_name: string): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  signature_help(file_ind: number, line_number: number, column: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  definition(file_ind: number, line_number: number, column: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  type_definition(file_ind: number, line_number: number, column: number): any;
/**
* @param {number} file_ind
* @returns {any}
*/
  document_symbols(file_ind: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @param {string} ch
* @returns {any}
*/
  type_formatting(file_ind: number, line_number: number, column: number, ch: string): any;
/**
* @param {number} file_ind
* @returns {any}
*/
  folding_ranges(file_ind: number): any;
/**
* @param {number} file_ind
* @param {number} line_number
* @param {number} column
* @returns {any}
*/
  goto_implementation(file_ind: number, line_number: number, column: number): any;
}
/**
*/
export class wbg_rayon_PoolBuilder {
  free(): void;
/**
* @returns {number}
*/
  numThreads(): number;
/**
* @returns {number}
*/
  receiver(): number;
/**
*/
  build(): void;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly start: () => void;
  readonly __wbg_worldstate_free: (a: number) => void;
  readonly worldstate_new: () => number;
  readonly worldstate_load: (a: number, b: number, c: number) => void;
  readonly worldstate_init: (a: number, b: number, c: number, d: number, e: number, f: number, g: number, h: number, i: number, j: number, k: number, l: number, m: number, n: number, o: number, p: number, q: number, r: number, s: number, t: number, u: number, v: number, w: number, x: number, y: number) => void;
  readonly worldstate_update: (a: number, b: number, c: number, d: number, e: number) => number;
  readonly worldstate_inlay_hints: (a: number, b: number) => number;
  readonly worldstate_completions: (a: number, b: number, c: number, d: number) => number;
  readonly worldstate_hover: (a: number, b: number, c: number, d: number) => number;
  readonly worldstate_code_lenses: (a: number, b: number) => number;
  readonly worldstate_references: (a: number, b: number, c: number, d: number, e: number) => number;
  readonly worldstate_prepare_rename: (a: number, b: number, c: number, d: number) => number;
  readonly worldstate_rename: (a: number, b: number, c: number, d: number, e: number, f: number) => number;
  readonly worldstate_signature_help: (a: number, b: number, c: number, d: number) => number;
  readonly worldstate_definition: (a: number, b: number, c: number, d: number) => number;
  readonly worldstate_type_definition: (a: number, b: number, c: number, d: number) => number;
  readonly worldstate_document_symbols: (a: number, b: number) => number;
  readonly worldstate_type_formatting: (a: number, b: number, c: number, d: number, e: number) => number;
  readonly worldstate_folding_ranges: (a: number, b: number) => number;
  readonly worldstate_goto_implementation: (a: number, b: number, c: number, d: number) => number;
  readonly __wbg_wbg_rayon_poolbuilder_free: (a: number) => void;
  readonly wbg_rayon_poolbuilder_numThreads: (a: number) => number;
  readonly wbg_rayon_poolbuilder_receiver: (a: number) => number;
  readonly wbg_rayon_poolbuilder_build: (a: number) => void;
  readonly wbg_rayon_start_worker: (a: number) => void;
  readonly initThreadPool: (a: number) => number;
  readonly memory: WebAssembly.Memory;
  readonly __wbindgen_malloc: (a: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number) => number;
  readonly __wbindgen_free: (a: number, b: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __wbindgen_thread_destroy: () => void;
  readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
* @param {WebAssembly.Memory} maybe_memory
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput, maybe_memory?: WebAssembly.Memory): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
* @param {WebAssembly.Memory} maybe_memory
*
* @returns {Promise<InitOutput>}
*/
export default function init (module_or_path?: InitInput | Promise<InitInput>, maybe_memory?: WebAssembly.Memory): Promise<InitOutput>;
