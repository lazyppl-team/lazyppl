import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { decode } from '../protocol.js';
import { fileURLToPath } from 'node:url';

const [wasm, nativeOutput] = process.argv.slice(2);
assert.ok(wasm && nativeOutput, 'Pass the rebuilt Wasm binary and the current native program-43-3 JSONL output.');
const reference = JSON.parse(readFileSync(new URL('./reference/program-seed43.json', import.meta.url), 'utf8'));
const samples = output => decode('program', { count: 3 }, output.trim().split('\n').map(JSON.parse)).samples;
const native = samples(readFileSync(nativeOutput, 'utf8'));
const output = execFileSync(
  process.execPath,
  [fileURLToPath(new URL('./run-wasi.mjs', import.meta.url)), wasm, ...reference.arguments],
  { encoding: 'utf8' }
);
const rows = samples(output);

function compare(label, actual, expected) {
  assert.equal(actual.length, expected.length, `${label}: got ${actual.length} rows; reference has ${expected.length}`);
  let maxScaledError = 0;
  for (let i = 0; i < expected.length; i++) {
    for (const key of ['program', 'x', 'y']) {
      const scale = Math.max(1, Math.abs(expected[i][key]));
      // Retain the existing 1e-10 relative/absolute bound across IEEE-754 backends.
      // Row counts, order and program identifiers are also checked, rather than fitted to a run.
      const tolerance = 1e-10 * scale;
      const error = Math.abs(actual[i][key] - expected[i][key]);
      maxScaledError = Math.max(maxScaledError, error / scale);
      assert.ok(error <= tolerance,
        `${label}, row ${i}, field ${key}: ${actual[i][key]} differs from ${expected[i][key]}`);
    }
  }
  console.log(`${label}: ${actual.length} rows; maximum scaled error ${maxScaledError}`);
}
compare('Current native versus recorded native', native, reference.samples);
compare('Fresh Wasm versus current native', rows, native);
console.log(`Program seed 43: three posterior functions agree; reference source ${reference.source_revision}.`);
