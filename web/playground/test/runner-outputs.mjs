import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { decode } from '../protocol.js';

const [native, wasm, only] = process.argv.slice(2);
assert.ok(native, 'Pass the native playground executable, and optionally its Wasm build.');
const wasi = fileURLToPath(new URL('./run-wasi.mjs', import.meta.url));
const run = (command, args) => execFileSync(command, args, {
  encoding: 'utf8', timeout: 180_000, maxBuffer: 16 * 1024 * 1024,
}).trim().split('\n').map(JSON.parse);
const execute = args => run(native, args);
const withoutTiming = records => records.filter(row => row.kind !== 'timing');

function poisson(rows) {
  const traces = rows.filter(row => row.kind === 'trace');
  assert.equal(traces.length, 1, 'Poisson must emit its sampled points and forced tree addresses.');
  const trace = traces[0];
  assert.equal(trace.count, trace.points.length);
  assert.ok(trace.points.every((x, i) => x > (trace.points[i - 1] ?? 0) && x < 1));
  assert.ok(trace.next_point >= 1, 'The first point outside the viewport stops the count.');
  assert.ok(trace.addresses.length >= trace.count + 1, 'Count also forces the first excluded arrival.');
  assert.equal(new Set(trace.addresses.map(JSON.stringify)).size, trace.addresses.length);
  assert.ok(trace.addresses.every(path => Array.isArray(path) && path.every(i => Number.isInteger(i) && i >= 0)));
  assert.equal(trace.uniforms.length, trace.addresses.length);
  assert.ok(trace.uniforms.every(u => u > 0 && u < 1));
  const histogram = rows.filter(row => row.kind === 'sample');
  assert.ok(histogram.some(row => row.count === trace.count), 'The traced count is one of the histogram draws.');
  assert.ok(Math.abs(histogram.reduce((s, row) => s + row.frequency, 0) - 1) < 1e-9);
}

function piecewise(rows, count) {
  assert.deepEqual(rows.filter(row => row.kind === 'observation').map(({ x, y }) => [x, y]),
    [[0, 0.6], [1, 0.7], [2, 1.2], [3, 3.2], [4, 6.8], [5, 8.2], [6, 8.4]]);
  const samples = rows.filter(row => row.kind === 'sample');
  assert.equal(samples.length, count);
  let discontinuities = 0;
  for (const [draw, row] of samples.entries()) {
    assert.equal(row.draw, draw);
    assert.equal(row.pieces.length, row.change_points.length + 1);
    assert.equal(row.values.length, 31);
    const boundaries = [0, ...row.change_points, 6];
    for (let i = 0; i < row.pieces.length; i++) {
      const p = row.pieces[i];
      assert.equal(p.x_lower, boundaries[i]);
      assert.equal(p.x_upper, boundaries[i + 1]);
      assert.ok(p.x_upper > p.x_lower);
      assert.ok(Number.isFinite(p.y_lower) && Number.isFinite(p.y_upper));
      if (i > 0 && Math.abs(p.y_lower - row.pieces[i - 1].y_upper) > 1e-6) discontinuities++;
    }
    for (let i = 0; i < row.values.length; i++) {
      const x = i / 5;
      const p = row.pieces.find(p => x <= p.x_upper);
      const y = p.y_lower + (p.y_upper - p.y_lower) * (x - p.x_lower) / (p.x_upper - p.x_lower);
      assert.ok(Math.abs(y - row.values[i]) <= 1e-10 * Math.max(1, Math.abs(y)),
        'Line pieces must reproduce the posterior function at every displayed grid point.');
    }
  }
  assert.ok(discontinuities > 0, 'The seeded piecewise posterior must contain a change of line.');
}

// Each posterior function's log_likelihood is the natural log of its Metropolis-Hastings weight: the Gaussian
// density of the seven observations around the function, with the noise that the demo scores (RegressionDemo.regress
// with the requested sigma, 0.1 for the piecewise model, 0.3 in GaussianProcessDemo.example and WienerDemo.example).
const NOISE = { piecewise: 0.1, gp: 0.3, wiener: 0.3 };
function likelihoods(kind, rows, parameter) {
  const sigma = kind === 'regression' ? Number(parameter) : NOISE[kind];
  // Regression emits its line's values at 0 and 6; the other demos emit the function on the grid x = i/5.
  const at = kind === 'regression' ? (row, x) => row.f0 + (row.f6 - row.f0) * x / 6 : (row, x) => row.values[5 * x];
  const observations = rows.filter(row => row.kind === 'observation');
  const fits = rows.filter(row => row.kind === 'sample').map(row => {
    const residuals = observations.map(({ x, y }) => y - at(row, x));
    const expected = residuals.reduce((total, r) => total - 0.5 * Math.log(2 * Math.PI) - Math.log(sigma) - 0.5 * (r / sigma) ** 2, 0);
    assert.ok(Math.abs(row.log_likelihood - expected) <= 1e-9 * Math.max(1, Math.abs(expected)),
      `${kind} draw ${row.draw}: log_likelihood ${row.log_likelihood} is not the Gaussian log-likelihood ${expected} of the observations`);
    return { logLikelihood: row.log_likelihood, squares: residuals.reduce((total, r) => total + r * r, 0) };
  });
  if (kind === 'piecewise') {
    const ranked = [...fits].sort((a, b) => a.logLikelihood - b.logLikelihood);
    assert.ok(ranked.at(-1).squares <= ranked[0].squares,
      'The most likely piecewise function must fit the observations at least as well as the least likely one.');
  }
}

function programLikelihoods(rows) {
  const observations = rows.filter(row => row.kind === 'observation');
  assert.equal(observations.length, 7);
  for (const row of rows.filter(row => row.kind === 'expression')) {
    assert.equal(row.observation_values?.length, observations.length,
      `program ${row.program}: expected a function value at each observation`);
    const expected = observations.reduce((total, { y }, i) => {
      const residual = (y - row.observation_values[i]) / 0.25;
      return total - 0.5 * Math.log(2 * Math.PI) - Math.log(0.25) - 0.5 * residual ** 2;
    }, 0);
    assert.ok(Math.abs(row.log_likelihood - expected) <= 1e-9 * Math.max(1, Math.abs(expected)),
      `program ${row.program}: log_likelihood ${row.log_likelihood} is not the Gaussian log-likelihood ${expected} of the observations`);
  }
}

function compare(actual, expected, location = 'result') {
  if (typeof expected === 'number') {
    assert.ok(Number.isFinite(actual) && Math.abs(actual - expected) <= 1e-10 * Math.max(1, Math.abs(expected)),
      `${location}: Wasm ${actual} differs from native ${expected}`);
  } else if (Array.isArray(expected)) {
    assert.ok(Array.isArray(actual), location);
    assert.equal(actual.length, expected.length, location);
    expected.forEach((value, i) => compare(actual[i], value, `${location}[${i}]`));
  } else if (expected && typeof expected === 'object') {
    assert.deepEqual(Object.keys(actual).sort(), Object.keys(expected).sort(), location);
    for (const key of Object.keys(expected)) compare(actual[key], expected[key], `${location}.${key}`);
  } else assert.equal(actual, expected, location);
}

const runners = [
  ['poisson'], ['regression', '0.5'], ['piecewise', '3'], ['gp', '5'], ['wiener', '5'],
  ['clustering', '1000'], ['irm', '1000'], ['program', '3'], ['ibp', '1000'],
  ['graph', '1000'], ['mondrian', '5'],
];
if (!only) {
  const [settings] = execute(['--settings']);
  assert.deepEqual(Object.keys(settings).sort(), runners.map(([kind]) => kind).sort());
  if (wasm && wasm !== '-') compare(run(process.execPath, [wasi, wasm, '--settings']), [settings], 'settings');
}
for (const [kind, ...parameters] of runners.filter(([kind]) => !only || kind === only)) {
  for (const seed of ['43', '44']) {
    const args = [kind, seed, ...parameters];
    const records = execute(args);
    const rows = withoutTiming(records);
    decode(kind, { count: Number(parameters[0]) }, records);
    if (kind === 'poisson') {
      poisson(rows);
      assert.deepEqual(withoutTiming(execute(args)), rows, 'Same seed must reproduce points, addresses and counts.');
    }
    if (kind === 'piecewise') piecewise(rows, Number(parameters[0]));
    if (kind === 'program') programLikelihoods(rows);
    if (['regression', 'piecewise', 'gp', 'wiener'].includes(kind)) likelihoods(kind, rows, parameters[0]);
    if (wasm && wasm !== '-') {
      const wasmRecords = run(process.execPath, [wasi, wasm, ...args]);
      decode(kind, { count: Number(parameters[0]) }, wasmRecords);
      if (kind === 'program') programLikelihoods(withoutTiming(wasmRecords));
      compare(withoutTiming(wasmRecords), rows);
      if (kind === 'poisson') assert.deepEqual(
        withoutTiming(run(process.execPath, [wasi, wasm, ...args])), withoutTiming(wasmRecords),
        'Same seed must reproduce Poisson points and tree addresses in Wasm.');
    }
    console.log(`PASS ${args.join(' ')} (${rows.length} records)${wasm && wasm !== '-' ? ': native/Wasm agree' : ''}`);
  }
}
