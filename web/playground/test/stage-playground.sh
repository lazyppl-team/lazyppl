#!/bin/bash
# Stage a browser directory from the built files and verified editor examples.
set -euo pipefail
if [ "$#" -ne 7 ]; then
    echo "usage: stage-playground.sh PLAYGROUND WASI EXAMPLES NATIVE_PLAN WASM_PLAN TOOLCHAIN OUTPUT" >&2
    exit 2
fi
playground=$1
wasi=$2
examples=$3
native_plan=$4
wasm_plan=$5
toolchain=$6
output=$7

# An existing output must still match its manifest before it is replaced.
if [ -d "$output" ]; then
    (cd "$output" && sha256sum -c SHA256SUMS)
    rm -r -- "$output"
fi
mkdir "$output"
cp -a "$examples" "$output/editor-examples"
read -r examples_hash _ < <(sha256sum "$output/editor-examples/examples.json")
physics_revision=$(jq -r '.physics_source_revision' "$output/editor-examples/examples.json")
jq --arg hash "$examples_hash" --arg physics_revision "$physics_revision" '.editor_examples_manifest_sha256 = $hash | .physics_source_revision = $physics_revision' "$playground/build.json" > "$output/build.json"
cp "$playground/index.html" "$playground"/*.js "$playground"/*.css "$playground/playground.wasm" "$output/"
cp -a "$wasi" "$output/wasi"
cp "$native_plan" "$output/native-plan.json"
cp "$wasm_plan" "$output/wasm-plan.json"
cp "$toolchain" "$output/toolchain.json"
(cd "$output" && sha256sum index.html ./*.js ./*.css playground.wasm wasi/* ./*.json editor-examples/* > SHA256SUMS)
printf 'Staged verified playground: %s\n' "$output"
