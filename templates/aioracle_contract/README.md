# "AI Oracle" Smart Contract Project

This is a sample AI Oracle smart contract which uses the cosmwasm contract to
enable richer interactions between Rust and AI APIs such as communicating with
computer vision services rather than just numbers.

Typically `wasm` is paired with a bundler but here we're not using a
bundler so you can poke around all the raw output!

Some files you may be interested in are:

- `src/lib.rs` - this is the library
- `contract.rs` - this is the main implementation
- `msg.rs`. Here you'll define the structures to communicate with contract ABI.
- `state.rs`. Here you'll define the world state of the contract.
  `error.rs`. Here you'll define errors.

When building the project you'll get `artifacts/cv009.wasm`, the generated wasm
filtered through the `wasmer` compiler, as well as `artifacts/checksums.txt`.
