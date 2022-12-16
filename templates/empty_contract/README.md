# "empty" Smart Contract Project

This is a sample Datasource smart contract which uses the cosmwasm contract to
enable richer interactions between Rust and AI APIs such as communicating with
computer vision services rather than just numbers.

Typically `wasm` is paired with a bundler but here we're not using a
bundler so you can poke around all the raw output!

Some files you may be interested in are:

- `contract.rs`. This is the main implementation
- `error.rs`. Here you'll define errors.
- `lib.rs`. This is the library
- `msg.rs`. Here you'll define the structures to communicate with contract ABI.
- `state.rs`. Storage for smart contract

When building the project you'll get `artifacts/cv009.wasm`, the generated wasm
filtered through the `wasmer` compiler, as well as `artifacts/checksums.txt`.
