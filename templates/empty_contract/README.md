# "empty" Smart Contract Project

This is a sample Datasource smart contract which uses the cosmwasm contract to
enable richer interactions between Rust and AI APIs such as communicating with
computer vision services rather than just numbers.

Typically `wasm` is paired with a bundler but here we're not using a
bundler so you can poke around all the raw output!

Some files you may be interested in are:

- `src/lib.rs` - this is the contract implementation

When building the project you'll get `artifacts/cv009.wasm`, the generated wasm
filtered through the `wasmer` compiler, as well as `artifacts/checksums.txt`.
