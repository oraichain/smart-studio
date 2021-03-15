# Oraichain Studio

To build Oraichain Studio whenever a file changes run:

```bash
yarn build-watch
# to build client
# template:
yarn templates server/fiddles/dist/templates
DIST_FOLDER=server/fiddles/dist yarn build
```

To start a dev web server run:

```
yarn start
```

Before submitting a pull request run:

```
yarn test
```

The fiddle server part:

```bash
cd server
docker-compose exec app bash
yarn start:dev
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
