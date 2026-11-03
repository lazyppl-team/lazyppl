#!/bin/bash
# Build both targets, compare real inference, and stage the exact browser directory for tests/upload.
set -euo pipefail
cd "$(dirname "$0")/.."
: "${SOURCE_REVISION:?Set the source commit being built}" "${GHC_WASM_PREFIX:?Set an absolute toolchain installation directory}"
[[ "$SOURCE_REVISION" =~ ^[0-9a-f]{40}$ ]]
node ../wasi/check-integrity.mjs

# Build native before sourcing the cross toolchain: its env sets CC/AR to Wasm tools.
cabal update
cabal build --builddir=dist-native exe:playground
cabal test --builddir=dist-native single-site-test geometric-graph-test
native=$(cabal list-bin --builddir=dist-native exe:playground)
"$native" program 43 3 > dist-native/program-seed43.jsonl

# The old checksum-pinned bootstrap downloads master. Fetch the actual upstream revision instead.
# Keep the previously selected metadata revision for reproducible toolchain selection.
meta_revision=8079eb96bbc63823d99ad8acaff0765532625474
meta_source="${GHC_WASM_PREFIX}-source"
if [ ! -d "$meta_source/.git" ]; then
    git init --quiet "$meta_source"
    git -C "$meta_source" fetch --depth=1 https://github.com/haskell-wasm/ghc-wasm-meta.git "$meta_revision"
    git -C "$meta_source" switch --detach FETCH_HEAD
fi
[ "$(git -C "$meta_source" rev-parse HEAD)" = "$meta_revision" ]
if [ ! -f "$GHC_WASM_PREFIX/env" ]; then
    # Upstream setup replaces PREFIX; a partial or unrelated installation requires an explicit cleanup.
    [ ! -e "$GHC_WASM_PREFIX" ] || { echo "Refusing to replace an existing toolchain directory" >&2; exit 1; }
    PREFIX="$GHC_WASM_PREFIX" FLAVOUR=9.14 bash "$meta_source/setup.sh"
    printf '%s\n' "$meta_revision" > "$GHC_WASM_PREFIX/ghc-wasm-meta-revision"
fi
[ "$(< "$GHC_WASM_PREFIX/ghc-wasm-meta-revision")" = "$meta_revision" ]
# shellcheck disable=SC1091 # Written by upstream setup.sh at the installation directory supplied by the caller.
. "$GHC_WASM_PREFIX/env"
wasm32-wasi-cabal update
wasm32-wasi-cabal build exe:playground test:single-site-test test:geometric-graph-test
cp "$(wasm32-wasi-cabal list-bin exe:playground)" playground.wasm
node test/run-wasi.mjs "$(wasm32-wasi-cabal list-bin test:single-site-test)"
node test/run-wasi.mjs "$(wasm32-wasi-cabal list-bin test:geometric-graph-test)"
node test/program-inference.mjs playground.wasm dist-native/program-seed43.jsonl
node test/poisson-seeded.mjs playground.wasm
node test/runner-outputs.mjs "$native" playground.wasm

read -r wasm_hash _ < <(sha256sum playground.wasm)
read -r native_plan_hash _ < <(sha256sum dist-native/cache/plan.json)
read -r wasm_plan_hash _ < <(sha256sum dist-newstyle/cache/plan.json)
jq -n --arg source_revision "$SOURCE_REVISION" --arg wasm_sha256 "$wasm_hash"     --arg native_plan_sha256 "$native_plan_hash" --arg wasm_plan_sha256 "$wasm_plan_hash"     --arg native_compiler "$(jq -r '."compiler-id"' dist-native/cache/plan.json)"     --arg wasm_compiler "$(jq -r '."compiler-id"' dist-newstyle/cache/plan.json)"     --arg ghc_wasm_meta_revision "$(git -C "$meta_source" rev-parse HEAD)"     '$ARGS.named' > build.json

examples_dir=$(mktemp -d)
trap 'rm -r -- "$examples_dir"' EXIT
node ../editor/generate-examples.mjs generate "$examples_dir" "$SOURCE_REVISION"
node ../editor/generate-examples.mjs check "$examples_dir" "$SOURCE_REVISION"
bash test/stage-playground.sh "$PWD" "$PWD/../wasi" "$examples_dir" \
    "$PWD/dist-native/cache/plan.json" "$PWD/dist-newstyle/cache/plan.json" \
    "$meta_source/autogen.json" "$PWD/dist-playground"
cp dist-playground/build.json build.json
