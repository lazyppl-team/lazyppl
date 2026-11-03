import { test, expect } from '@playwright/test';

test('compiled worker exports Poisson traversal and piecewise posterior lines', async ({ page }) => {
  await page.goto('/index.html');
  const results = await page.evaluate(async () => {
    const { decode } = await import('/protocol.js');
    const output = [];
    for (const [kind, args, request] of [
      ['poisson', ['poisson', '43'], {}],
      ['piecewise', ['piecewise', '43', '3'], { count: 3 }],
    ]) {
      const records = await new Promise((resolve, reject) => {
        const worker = new Worker('/worker.js', { type: 'module' });
        worker.onerror = event => { worker.terminate(); reject(new Error(event.message)); };
        worker.onmessage = ({ data }) => {
          worker.terminate();
          if (data.error) reject(new Error(data.error));
          else resolve(data.records);
        };
        worker.postMessage({ kind, args });
      });
      const result = decode(kind, request, records);
      output.push(kind === 'poisson'
        ? { kind, count: result.trace.count, points: result.trace.points.length, sites: result.trace.addresses.length }
        : { kind, observations: result.observations.length, draws: result.samples.length,
          changes: result.samples.reduce((sum, row) => sum + row.change_points.length, 0) });
    }
    return output;
  });
  expect(results[0].points).toBe(results[0].count);
  expect(results[0].sites).toBeGreaterThanOrEqual(results[0].count + 1);
  expect(results[1]).toMatchObject({ kind: 'piecewise', observations: 7, draws: 3 });
  expect(results[1].changes).toBeGreaterThan(0);
});

const EXPECTED_SETTINGS = {
  poisson: {},
  regression: { min: 0.1, max: 3, step: 0.1, original: 0.5 },
  gp: { min: 5, max: 100, step: 1, original: 20 },
  wiener: { min: 5, max: 100, step: 1, original: 20 },
  clustering: { min: 1000, max: 50000, step: 1000, original: 20000 },
  irm: { min: 1000, max: 50000, step: 1000, original: 10000 },
  program: { min: 1, max: 20, step: 1, original: 3 },
  ibp: { min: 1000, max: 200000, step: 1000, original: 100000 },
  graph: { min: 1000, max: 50000, step: 1000, original: 20000 },
  mondrian: { min: 0.5, max: 10, step: 0.5, original: 5 },
};

test('populates form parameter bounds from --settings and rejects out-of-range inputs', async ({ page }) => {
  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });

  for (const [kind, info] of Object.entries(EXPECTED_SETTINGS)) {
    const inputId = kind === 'regression' ? 'noise' : `${kind}-count`;
    if (Object.keys(info).length === 0) {
      await expect(page.locator(`#${inputId}`)).toHaveCount(0);
      continue;
    }
    const locator = page.locator(`#${inputId}`);
    await expect(locator).toHaveAttribute('min', String(info.min));
    await expect(locator).toHaveAttribute('max', String(info.max));
    await expect(locator).toHaveAttribute('step', String(info.step));
    const defaultValue = await locator.evaluate(el => Number(el.defaultValue));
    expect(defaultValue).toBe(info.original);
  }

  const boundaryCases = [
    { inputId: 'program-count', raw: '0', valid: false },
    { inputId: 'program-count', raw: '1', valid: true },
    { inputId: 'program-count', raw: '2', valid: true },
    { inputId: 'program-count', raw: '3', valid: true },
    { inputId: 'program-count', raw: '20', valid: true },
    { inputId: 'program-count', raw: '21', valid: false },
    { inputId: 'program-count', raw: '2.5', valid: false },
    { inputId: 'clustering-count', raw: '1500', valid: false },
    { inputId: 'mondrian-count', raw: '0.7', valid: false },
    { inputId: 'mondrian-count', raw: '0.5', valid: true },
  ];

  for (const { inputId, raw, valid } of boundaryCases) {
    const htmlValid = await page.locator(`#${inputId}`).evaluate((el, val) => {
      el.value = val;
      return el.checkValidity();
    }, raw);
    expect(htmlValid).toBe(valid);
  }

  await page.fill('#program-count', '0');
  await page.click('#program-run');
  await expect(page.locator('#program-result')).toHaveText('Ready.');
});

test('renders plots from named sample records and rejects a malformed producer field', async ({ page }) => {
  await page.route('**/worker.js', async route => {
    await route.fulfill({
      status: 200,
      contentType: 'text/javascript; charset=utf-8',
      body: `
        let mondrianCalls = 0;
        self.onmessage = ({ data }) => {
          if (data.kind === 'settings') {
            postMessage({ kind: 'settings', settings: {} });
          } else if (data.kind === 'poisson') {
            postMessage({
              kind: 'poisson',
              records: [
                { kind: 'trace', count: 0, points: [], next_point: 1.2, addresses: [[0]], uniforms: [0.3] },
                { kind: 'sample', count: 0, frequency: 1.0 },
                { kind: 'timing', elapsed_ms: 1.0 },
              ],
              runMs: 1,
            });
          } else if (data.kind === 'mondrian') {
            mondrianCalls += 1;
            const sample = mondrianCalls === 1
              ? { kind: 'sample', block: 0, x_lower: 0, x_upper: 1, y_lower: 0, y_upper: 1, value: 0.5 }
              : { kind: 'sample', block: 0, x_lower_renamed: 0, x_upper: 1, y_lower: 0, y_upper: 1, value: 0.5 };
            postMessage({
              kind: 'mondrian',
              records: [
                sample,
                { kind: 'timing', elapsed_ms: 1.0 },
              ],
              runMs: 1,
            });
          }
        };
      `,
    });
  });

  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 10000 });

  await page.click('#mondrian-run');
  await expect(page.locator('#mondrian-result')).toHaveAttribute('data-state', 'done', { timeout: 10000 });
  await expect(page.locator('#mondrian-plot rect')).toHaveCount(1);

  await page.click('#mondrian-run');
  await expect(page.locator('#mondrian-result')).toHaveAttribute('data-state', 'error', { timeout: 10000 });
  const svgHtml = await page.locator('#mondrian-plot').innerHTML();
  expect(svgHtml).not.toContain('NaN');
});

const KINDS = ['poisson', 'regression', 'gp', 'wiener', 'clustering', 'irm', 'program', 'ibp', 'graph', 'mondrian'];

for (const kind of KINDS) {
  test(`${kind}: a result that holds only its timing record fails visibly and draws nothing`, async ({ page }) => {
    await page.route('**/worker.js', async route => {
      await route.fulfill({
        status: 200,
        contentType: 'text/javascript; charset=utf-8',
        body: `
          self.onmessage = ({ data }) => {
            if (data.kind === 'settings') {
              postMessage({ kind: 'settings', settings: {} });
            } else {
              postMessage({ kind: data.kind, records: [{ kind: 'timing', elapsed_ms: 1.0 }], runMs: 1 });
            }
          };
        `,
      });
    });

    await page.goto('/index.html');
    const result = page.locator(`#${kind}-result`);
    await page.click(`#${kind}-run`);
    await expect(result).not.toHaveAttribute('data-state', 'running', { timeout: 10000 });

    const plot = page.locator(`#${kind}-plot`);
    if (kind !== 'poisson') {
      expect.soft(await plot.innerHTML(), `${kind} plot geometry`).not.toContain('NaN');
      expect.soft(await plot.evaluate(svg => svg.children.length), `${kind} plot elements`).toBe(2);
    }
    expect.soft(await result.textContent(), `${kind} result text`).not.toMatch(/posterior|inference steps|samples in|blocks at/);
    await expect(result).toHaveAttribute('data-state', 'error');
    await expect(result).toContainText('Could not run');
  });
}

test('recreates the worker after a load failure and completes the next run', async ({ page }) => {
  let workerRequests = 0;
  await page.route('**/worker.js', async route => {
    workerRequests += 1;
    if (workerRequests === 1) {
      await route.fulfill({
        status: 500,
        contentType: 'text/plain',
        body: 'Simulated transient network error loading worker.js',
      });
    } else {
      await route.continue();
    }
  });

  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'error', { timeout: 10000 });
  expect(workerRequests).toBe(1);

  await page.click('#poisson-run');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });
  expect(workerRequests).toBe(2);
  await expect(page.locator('#poisson-result')).toContainText('10,000 samples');
});

test('runs again after the WebAssembly download fails once', async ({ page }) => {
  let wasmRequests = 0;
  await page.route('**/playground.wasm', async route => {
    wasmRequests += 1;
    if (wasmRequests === 1) {
      await route.fulfill({ status: 503, contentType: 'text/plain', body: 'Simulated transient server error' });
    } else {
      await route.continue();
    }
  });

  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'error', { timeout: 10000 });
  await expect(page.locator('#poisson-result')).toContainText('Could not run');
  expect(wasmRequests).toBe(1);

  await page.click('#poisson-run');
  await expect(page.locator('#poisson-result')).not.toHaveAttribute('data-state', 'running', { timeout: 15000 });
  expect.soft(wasmRequests, 'downloads after the second Run').toBe(2);
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done');
  await expect(page.locator('#poisson-result')).toContainText('10,000 samples');
});

test('downloads the WebAssembly binary once for successive runs', async ({ page }) => {
  let wasmRequests = 0;
  await page.route('**/playground.wasm', async route => {
    wasmRequests += 1;
    await route.continue();
  });

  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });
  await page.click('#mondrian-run');
  await expect(page.locator('#mondrian-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });
  await page.click('#poisson-run');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });
  expect(wasmRequests).toBe(1);
});

test('cancels an in-flight run with Cancelled status and completes a subsequent run', async ({ page }) => {
  let workerLoads = 0;
  await page.route('**/worker.js', async route => {
    workerLoads += 1;
    const loadId = workerLoads;
    await route.fulfill({
      status: 200,
      contentType: 'text/javascript; charset=utf-8',
      body: `
        const loadId = ${loadId};
        self.onmessage = ({ data }) => {
          if (data.kind === 'settings') {
            postMessage({ kind: 'settings', settings: {} });
          } else if (data.kind === 'poisson') {
            postMessage({
              kind: 'poisson',
              records: [
                { kind: 'trace', count: 0, points: [], next_point: 1.2, addresses: [[0]], uniforms: [0.3] },
                { kind: 'sample', count: 0, frequency: 1.0 },
                { kind: 'timing', elapsed_ms: 1.0 },
              ],
              runMs: 1,
            });
          } else if (data.kind === 'wiener') {
            if (loadId === 1) {
              return;
            }
            postMessage({
              kind: 'wiener',
              records: [
                ...Array.from({ length: 7 }, (_, i) => ({ kind: 'observation', x: i, y: i * 0.5 })),
                ...Array.from({ length: 20 }, (_, draw) => ({
                  kind: 'sample',
                  draw,
                  values: Array.from({ length: 31 }, (_, i) => i * 0.1),
                  log_likelihood: -30 - draw,
                })),
                { kind: 'timing', elapsed_ms: 5.0 },
              ],
              runMs: 5,
            });
          }
        };
      `,
    });
  });

  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 10000 });

  await page.click('#wiener-run');
  await expect(page.locator('#wiener-result')).toHaveAttribute('data-state', 'running', { timeout: 5000 });
  await expect(page.locator('#cancel')).toBeEnabled();

  await page.click('#cancel');
  await expect(page.locator('#wiener-result')).toHaveAttribute('data-state', 'cancelled');
  await expect(page.locator('#wiener-result')).toHaveText('Computation cancelled.');
  await expect(page.locator('#wiener-run')).toBeEnabled();
  await expect(page.locator('#cancel')).toBeDisabled();

  await page.click('#wiener-run');
  await expect(page.locator('#wiener-result')).toHaveAttribute('data-state', 'done', { timeout: 10000 });
  await expect(page.locator('#wiener-result')).toContainText('posterior functions');
});

test('terminates active real Wasm inference and runs a fresh worker', async ({ page }) => {
  await page.goto('/index.html');
  await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });
  const first = page.workers()[0];
  let closed = false;
  first.on('close', () => { closed = true; });
  // Observe the real WASI entry, without replacing inference or its compiled executable.
  await first.evaluate(async () => {
    const { WASI } = await import('/wasi/index.js');
    const start = WASI.prototype.start;
    WASI.prototype.start = function (instance) {
      console.debug('Entering real Wasm inference');
      return start.call(this, instance);
    };
  });
  await page.fill('#ibp-count', '200000');
  const started = page.waitForEvent('console', message => message.text() === 'Entering real Wasm inference');
  await page.click('#ibp-run');
  await started;
  await expect(page.locator('#ibp-result')).toHaveAttribute('data-state', 'running');
  // Chromium allows two seconds before forcibly terminating busy worker execution.
  // Use Playwright's five-second assertion window for shutdown and test transport.
  // https://chromium.googlesource.com/chromium/src/+/refs/heads/main/third_party/blink/renderer/core/workers/worker_thread.cc
  const cancelledAt = Date.now();
  await page.click('#cancel');
  await expect.poll(() => closed, { timeout: 5000, message: 'The active Wasm worker must actually close' }).toBe(true);
  expect(Date.now() - cancelledAt).toBeLessThan(5000);
  await expect(page.locator('#ibp-result')).toHaveAttribute('data-state', 'cancelled');
  await page.fill('#wiener-count', '5');
  await page.click('#wiener-run');
  await expect(page.locator('#wiener-result')).toHaveAttribute('data-state', 'done', { timeout: 30000 });
  expect(page.workers()).not.toContain(first);
  await expect(page.locator('#ibp-result')).toHaveAttribute('data-state', 'cancelled');
  await expect(page.locator('#wiener-result')).toContainText('5 posterior functions');
  expect(await page.locator('#wiener-data').evaluate(el => JSON.parse(el.textContent).length)).toBe(5);
  await curves('wiener', 5)(page);
});

const REAL_RUNS = {
  poisson: { input: null, check: checkPoisson },
  regression: {
    input: null,
    check: async page => {
      await expect(page.locator('#regression-plot path.posterior')).toHaveCount(1000);
      await expect(page.locator('#regression-plot polygon.predictive')).toHaveCount(1);
      await expect(page.locator('#regression-plot circle.observation')).toHaveCount(7);
      await expect(page.locator('#regression-result')).toContainText('1,000 posterior functions');
    },
  },
  gp: { input: ['#gp-count', '5'], check: curves('gp', 5) },
  wiener: { input: ['#wiener-count', '5'], check: curves('wiener', 5) },
  clustering: {
    input: ['#clustering-count', '1000'],
    check: async page => {
      await expect(page.locator('#clustering-plot circle')).toHaveCount(10);
    },
  },
  irm: {
    input: ['#irm-count', '1000'],
    check: async page => {
      await expect(page.locator('#irm-plot rect')).toHaveCount(36);
      await expect(page.locator('#irm-description')).toContainText('Tom, Fred, Jim, Mary, Sue, Ann');
    },
  },
  program: {
    input: ['#program-count', '1'],
    check: async page => {
      await expect(page.locator('#program-plot path.function-draw')).toHaveCount(1);
      await expect(page.locator('#program-plot circle.observation')).toHaveCount(7);
    },
  },
  ibp: {
    input: ['#ibp-count', '1000'],
    check: async page => {
      const rects = await page.locator('#ibp-plot rect').count();
      expect(rects % 16, 'one cell per country and feature').toBe(0);
      await expect(page.locator('#ibp-description')).toContainText('China');
      await expect(page.locator('#ibp-description')).toContainText('Zimbabwe');
    },
  },
  graph: {
    input: ['#graph-count', '1000'],
    check: async page => {
      await expect(page.locator('#graph-plot circle')).toHaveCount(15);
    },
  },
  mondrian: {
    input: ['#mondrian-count', '0.5'],
    check: async page => {
      const area = await page.locator('#mondrian-plot rect').evaluateAll(rects =>
        rects.reduce((sum, rect) => sum + Number(rect.getAttribute('width')) * Number(rect.getAttribute('height')), 0)
      );
      expect(area / (310 * 310), 'blocks tile the unit square').toBeCloseTo(1, 6);
    },
  },
};

function curves(kind, count) {
  return async page => {
    await expect(page.locator(`#${kind}-plot path.function-draw`)).toHaveCount(count);
    await expect(page.locator(`#${kind}-plot circle.observation`)).toHaveCount(7);
    await expect(page.locator(`#${kind}-result`)).toContainText(`${count} posterior functions`);
  };
}

async function checkPoisson(page) {
  const lines = (await page.locator('#poisson-result').innerText()).split('\n');
  const frequencies = lines.filter(line => /^\d+\t/.test(line)).map(line => Number(line.split('\t')[1]));
  expect(frequencies.length, 'distinct counts').toBeGreaterThan(1);
  expect(frequencies.reduce((a, b) => a + b, 0)).toBeCloseTo(1, 9);
}

for (const kind of KINDS) {
  test(`${kind}: the real WebAssembly worker produces a result that the page draws`, async ({ page }) => {
    test.setTimeout(240000);
    await page.goto('/index.html');
    await expect(page.locator('#poisson-result')).toHaveAttribute('data-state', 'done', { timeout: 15000 });
    const { input, check } = REAL_RUNS[kind];
    if (input) await page.fill(...input);
    await page.click(`#${kind}-run`);
    await expect(page.locator(`#${kind}-result`)).toHaveAttribute('data-state', 'done', { timeout: 200000 });
    if (kind !== 'poisson') {
      expect(await page.locator(`#${kind}-plot`).innerHTML(), `${kind} plot geometry`).not.toContain('NaN');
    }
    await check(page);
  });
}
