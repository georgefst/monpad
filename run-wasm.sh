# TODO move this logic to Shake script,
# and move other files in this dir elsewhere

set -e

rm -rf dist/wasm
mkdir dist/wasm

wasm32-wasi-cabal --builddir=.build/hs/wasm build frontend

hs_wasm_path=$(wasm32-wasi-cabal --builddir=.build/hs/wasm list-bin -v0 frontend)

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input "$hs_wasm_path" --output dist/wasm/ghc_wasm_jsffi.js

wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/wasm/bin.wasm "$hs_wasm_path"

# TODO make these optional
# wasm-opt -Oz dist/wasm/bin.wasm -o dist/wasm/bin.wasm
# wasm-tools strip -o dist/wasm/bin.wasm dist/wasm/bin.wasm
# brotli --best dist/wasm/bin.wasm -o dist/wasm/bin.wasm.br

cp html/* dist/wasm/
cp js/* dist/wasm/
cp css/* dist/wasm/

# TODO remove?
python -m http.server 8002 -d ./dist/wasm
