import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const [candidate, prior] = process.argv.slice(2);
assert.ok(candidate, 'Pass the candidate Wasm runner and optionally the prior runner.');
const wasi = fileURLToPath(new URL('./run-wasi.mjs', import.meta.url));
// Pre-trace playground, built from 259098e after the newStdGen acquisition fix.
const expected = new Map([
  ['42', [[0, 0.3745], [1, 0.3664], [2, 0.1819], [3, 0.0594], [4, 0.0148], [5, 0.0022], [6, 0.0006], [7, 0.0002]]],
  ['43', [[0, 0.3743], [1, 0.3672], [2, 0.1769], [3, 0.0599], [4, 0.0183], [5, 0.0028], [6, 0.0005], [7, 0.0001]]],
]);

function histogram(binary, seed) {
  return execFileSync(process.execPath, [wasi, binary, 'poisson', seed], { encoding: 'utf8' })
    .trim().split('\n').map(line => JSON.parse(line)).filter(row => row.kind === 'sample')
    .map(({ count, frequency }) => [count, frequency]);
}

const failures = [];
for (const [seed, rows] of expected) {
  if (prior) assert.deepEqual(histogram(prior, seed), rows, `prior runner, seed ${seed}`);
  const actual = histogram(candidate, seed);
  if (JSON.stringify(actual) !== JSON.stringify(rows)) {
    failures.push(`seed ${seed}: expected ${JSON.stringify(rows)}, got ${JSON.stringify(actual)}`);
  } else console.log(`PASS Poisson seed ${seed}: histogram matches the prior runner`);
}
assert.deepEqual(failures, [], 'Poisson histograms must match the prior runner row by row');
