# Oraichain Studio

To build Oraichain Studio whenever a file changes run:

```bash
# run test crates
cargo test --package rust-pack --lib --release -- tests::initialize_state --exact --nocapture

# build rust
wasm-pack build crates/ra-wasm --target web

# for hot-reload in development, default is both: --output=rust&json
cargo run -p rust-pack --release -- ../cosmwasm --toolchain +1.51.0

# change toolchain for lib and install source code to reference
rustup toolchain add 1.51.0 --profile minimal && rustup +1.51.0 component add rust-src

# or using source code, should be version >= 1.47.0, minimum support is 1.42.0
svn export https://github.com/rust-lang/rust/tags/1.47.0/library rust_library/1.47.0
cargo run -p rust-pack --release -- ../cosmwasm --toolchain rust_library/1.47.0 --output rust

# to build client
# rebuild template
yarn templates
yarn build
SERVICE_URL=http://localhost:3000 yarn start
```

Before submitting a pull request run:

```
yarn test
```

### Credits

This project depends on several excellent libraries and tools:

- [Monaco Editor](https://github.com/Microsoft/monaco-editor) is used for rich text editing, tree views and context menus.

- [WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt) is used to assemble and disassemble `.wasm` files.

- [Binaryen](https://github.com/WebAssembly/binaryen/) is used to validate and optimize `.wasm` files.

- [Clang Format](https://github.com/tbfleming/cib) is used to format C/C++ files.

- [Cassowary.js](https://github.com/slightlyoff/cassowary.js/) is used to make split panes work.

- [Showdown](https://github.com/showdownjs/showdown) is used to automatically preview `.md` files.

- [Capstone.js](https://alexaltea.github.io/capstone.js/) is used to disassemble `x86` code.

- LLVM, Rust, Emscripten running server side.

- And of course: React, WebPack, TypeScript and TSLint.
