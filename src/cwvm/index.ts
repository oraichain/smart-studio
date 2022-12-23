import { eddsa } from 'elliptic';

export * from './memory';
export * from './backend';
export * from './instance';
export * from './environment';

import { default as init, Poseidon, curve_hash, groth16_verify } from '../../crates/cosmwasm-vm-js-zk/pkg';
// update zk wasm implementation
import { VMInstance } from './instance';

init().then(() => {
  const poseidon = new Poseidon();
  VMInstance.poseidon_hash = poseidon.hash.bind(poseidon);
  VMInstance.curve_hash = curve_hash;
  VMInstance.groth16_verify = groth16_verify;
});

globalThis.eddsa = () => globalThis._eddsa || (globalThis._eddsa = new eddsa('ed25519'));
