# LazyPPL browser playground

The playground is a web page that runs LazyPPL demos, compiled to WebAssembly (Wasm), in a Web Worker. Build `playground.wasm`, then serve this directory over HTTP.

## Building and testing

The build and the tests need native GHC, Cabal, Git, Node.js, npm, npx, jq, unzip, zstd and sha256sum. They also need network access to download the Wasm compiler and dependencies. All commands run from this directory.

The public repository does not include the browser editor; this build uses `../editor/generate-examples.mjs`, its drivers and the physics extractor to generate examples.

```sh
SOURCE_REVISION="$(git rev-parse HEAD)" \
GHC_WASM_PREFIX="$HOME/.ghc-wasm-lazyppl" \
bash test/build-playground.sh
```

To run the heap-layout regression test with native GHC:

```sh
cabal test single-site-test
```

Test `dist-playground/` before publishing:

```sh
npm --prefix test ci
npx --prefix test playwright install --with-deps chromium
LAZYPPL_PLAYGROUND_ROOT="$PWD/dist-playground" npm --prefix test test
```

`test/protocol.schema.json` states the shape of each demo's result, and `test/build-validators.mjs` generates `validators.js` from it. After you change the schema, regenerate `validators.js` and commit it:

```sh
npm --prefix test ci
npm --prefix test run validators
```

A test fails when `validators.js` is out of date, and `protocol.js` checks relations between records.

The [project README](../../README.md) links the library documentation and examples.
