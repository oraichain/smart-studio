# "mixer" Smart Contract Project

This is a sample Datasource smart contract which uses the cosmwasm zk Api to
enable anonymous transactions like Tornado cash Mixer.

Typically `wasm` is paired with a bundler but here we're not using a
bundler so you can poke around all the raw output!

Some files you may be interested in are:

- `contract.rs`. This is the main implementation
- `error.rs`. Here you'll define errors.
- `lib.rs`. This is the contract entry file.
- `msg.rs`. Here you'll define the structures to communicate with contract ABI.
- `state.rs`. Storage for smart contract

When building the project you'll get `artifacts/mixer.wasm`, the generated wasm
filtered through the `wasmer` compiler, as well as `artifacts/checksums.txt`.
