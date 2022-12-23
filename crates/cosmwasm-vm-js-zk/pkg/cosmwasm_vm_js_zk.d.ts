/* tslint:disable */
/* eslint-disable */
/**
* @param {Uint8Array} input
* @param {Uint8Array} proof
* @param {Uint8Array} vk
* @returns {boolean}
*/
export function groth16_verify(input: Uint8Array, proof: Uint8Array, vk: Uint8Array): boolean;
/**
* @param {Uint8Array} input
* @returns {Uint8Array}
*/
export function curve_hash(input: Uint8Array): Uint8Array;
/**
*/
export class Poseidon {
  free(): void;
/**
*/
  constructor();
/**
* @param {(Uint8Array)[]} inputs
* @returns {Uint8Array}
*/
  hash(inputs: (Uint8Array)[]): Uint8Array;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly groth16_verify: (a: number, b: number, c: number, d: number) => void;
  readonly curve_hash: (a: number) => number;
  readonly __wbg_poseidon_free: (a: number) => void;
  readonly poseidon_new: () => number;
  readonly poseidon_hash: (a: number, b: number, c: number, d: number) => void;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_malloc: (a: number) => number;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
