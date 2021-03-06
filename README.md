# Oraichain Studio

To build Oraichain Studio whenever a file changes run:

```bash
yarn build-server
# to build client
# rebuild template
yarn templates
DIST_FOLDER=server/fiddles/dist yarn build
```

To start a dev web server run:

```bash
# run fiddle server
GITHUB_CALLBACK_URL=http://localhost:8080 yarn start:dev
SERVICE_URL=//localhost:3000 WALLET_URL=//localhost:3001 LCD=https://testnet-lcd.orai.io yarn start
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
