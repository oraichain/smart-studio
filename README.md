# Oraichain Studio

To build Oraichain Studio whenever a file changes run:

```bash
# build rust
wasm-pack build crates/ra-wasm --target web
# default bundle to json change
COSMWASM_PATH=../cosmwasm cargo run -p rust-pack --release
# for hot-reload in development
OUTPUT_TYPE=rust|json COSMWASM_PATH=../cosmwasm cargo run -p rust-pack --release



# to build client
# rebuild template
yarn templates
DIST_FOLDER=../smart-studio-server/dist yarn build
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
