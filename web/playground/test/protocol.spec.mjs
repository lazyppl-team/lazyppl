import { test, expect } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { generate, validatorsUrl } from './build-validators.mjs';

const range = (n, f) => Array.from({ length: n }, (_, i) => f(i));
const timing = { kind: 'timing', elapsed_ms: 12.5 };
const observations = range(7, i => ({ kind: 'observation', x: i, y: i / 2 }));
const grid = () => range(31, i => i / 10);
const PEOPLE = ['Tom', 'Fred', 'Jim', 'Mary', 'Sue', 'Ann'];

// One complete result per demo, written from the shapes that hs/Main.hs documents, not from the validator.
const COMPLETE = {
  poisson: () => [
    { kind: 'trace', count: 1, points: [0.5], next_point: 1.25,
      addresses: [[0], [1]], uniforms: [Math.exp(-0.5), Math.exp(-0.75)] },
    { kind: 'sample', count: 0, frequency: 0.25 },
    { kind: 'sample', count: 1, frequency: 0.75 },
    timing,
  ],
  regression: () => [
    ...observations,
    ...range(1000, draw => ({ kind: 'sample', draw, f0: 0.1, f6: 8.2, predictive: grid(), log_likelihood: -12.5 - draw / 100 })),
    timing,
  ],
  piecewise: () => [
    ...observations,
    { kind: 'sample', draw: 0, change_points: [3],
      pieces: [
        { x_lower: 0, x_upper: 3, y_lower: 0, y_upper: 3 },
        { x_lower: 3, x_upper: 6, y_lower: 6, y_upper: 9 },
      ], values: range(31, i => i / 5 <= 3 ? i / 5 : i / 5 + 3), log_likelihood: -840.25 },
    timing,
  ],
  gp: () => [...observations, ...range(3, draw => ({ kind: 'sample', draw, values: grid(), log_likelihood: -40 - draw })), timing],
  wiener: () => [...observations, ...range(3, draw => ({ kind: 'sample', draw, values: grid(), log_likelihood: -30 - draw })), timing],
  clustering: () => [
    ...range(10, point => ({
      kind: 'sample',
      point,
      x: point,
      y: point / 2,
      cluster: 0.25,
      centre_x: 4,
      centre_y: 5,
      sigma: 1.5,
    })),
    timing,
  ],
  irm: () => [
    ...range(6, person => ({
      kind: 'sample',
      person,
      label: PEOPLE[person],
      probabilities: range(6, other => (other === person ? 1 : 0.5)),
    })),
    timing,
  ],
  program: () => [
    ...observations,
    ...range(2, program => ({ kind: 'expression', program, expression: `(${program}.0 + 1.0 . x)`,
      observation_values: range(7, x => x + program), log_likelihood: [-196.48850920459344, -122.88850920459345][program] })),
    ...range(2, program => [-0.25, 3, 6.2].map(x => ({ kind: 'sample', program, x, y: x + program }))).flat(),
    timing,
  ],
  ibp: () => [
    ...range(16, country => ({ kind: 'sample', country, label: `Country ${country}`, features: [1, 0, 1] })),
    timing,
  ],
  graph: () => [
    { kind: 'graph_stat', matches: 96, theta: 0.6 },
    ...range(15, vertex => ({ kind: 'sample', vertex, edges: range(15, other => (vertex + other) % 2) })),
    timing,
  ],
  mondrian: () => [
    { kind: 'sample', block: 0, x_lower: 0, x_upper: 1, y_lower: 0, y_upper: 0.5, value: 0.3 },
    { kind: 'sample', block: 1, x_lower: 0, x_upper: 1, y_lower: 0.5, y_upper: 1, value: 0.9 },
    timing,
  ],
};
const REQUEST = { gp: { count: 3 }, wiener: { count: 3 }, piecewise: { count: 1 }, program: { count: 2 } };
const KINDS = Object.keys(COMPLETE);
const requestFor = kind => REQUEST[kind] ?? {};

const without = (records, matches) => records.filter(record => !matches(record));
const change = (records, matches, update) => records.map(record => (matches(record) ? { ...record, ...update } : record));
const rename = (records, matches, from, to) =>
  records.map(record => {
    if (!matches(record)) return record;
    const { [from]: value, ...rest } = record;
    return { ...rest, [to]: value };
  });
const omit = (records, matches, field) =>
  records.map(record => {
    if (!matches(record)) return record;
    const { [field]: _, ...rest } = record;
    return rest;
  });
const draw = n => record => record.kind === 'sample' && record.draw === n;
const flip = (edges, i) => edges.map((edge, j) => (j === i ? 1 - edge : edge));

// Each case breaks one rule of the contract. The pattern names where the page reports it.
const REJECTED = [
  ['regression', 'a missing draw', r => r.slice(0, 7).concat(r.slice(8)), /\/samples must NOT have fewer than 1000/],
  ['regression', 'a grid of 30 positions', r => change(r, draw(5), { predictive: grid().slice(1) }), /\/samples\/5\/predictive/],
  ['regression', 'six observations', r => r.slice(1), /\/observations must NOT have fewer than 7/],
  ['regression', 'a renamed field', r => rename(r, draw(0), 'f0', 'f0_renamed'), /\/samples\/0 must have required property 'f0'/],
  ['regression', 'an extra field', r => change(r, draw(0), { extra: 1 }), /\/samples\/0 must NOT have additional properties 'extra'/],
  ['regression', 'an infinite number', r => change(r, draw(0), { f6: Infinity }), /\/samples\/0\/f6 must be number/],
  ['regression', 'a string for a number', r => change(r, draw(0), { f0: '0.1' }), /\/samples\/0\/f0 must be number/],
  ['regression', 'a second timing record', r => [...r, timing], /\/timings must NOT have more than 1/],
  ['regression', 'a record of another kind', r => [...r, { kind: 'debug', text: 'x' }], /additional properties 'debugs'/],
  ['poisson', 'frequencies that sum to 0.9', r => change(r, record => record.kind === 'sample' && record.count === 1, { frequency: 0.65 }), /do not sum to 1/],
  ['poisson', 'counts that are not increasing', r => change(r, record => record.kind === 'sample' && record.count === 1, { count: 0 }), /not increasing/],
  ['poisson', 'a missing traversal', r => without(r, record => record.kind === 'trace'), /required property 'traces'/],
  ['poisson', 'a count inconsistent with the points', r => change(r, record => record.kind === 'trace', { count: 2 }), /point count/],
  ['poisson', 'a point outside the viewport', r => change(r, record => record.kind === 'trace', { points: [1.2] }), /\/traces\/0\/points\/0/],
  ['piecewise', 'a gap between pieces', r => change(r, draw(0), { pieces: [
    { x_lower: 0, x_upper: 3, y_lower: 0, y_upper: 3 },
    { x_lower: 3.1, x_upper: 6, y_lower: 6, y_upper: 9 },
  ] }), /boundaries/],
  ['regression', 'a posterior function without its log-likelihood', r => omit(r, draw(4), 'log_likelihood'), /\/samples\/4 must have required property 'log_likelihood'/],
  ['regression', 'a log-likelihood given as a string', r => change(r, draw(4), { log_likelihood: '-12.5' }), /\/samples\/4\/log_likelihood must be number/],
  ['piecewise', 'a posterior function without its log-likelihood', r => omit(r, draw(0), 'log_likelihood'), /\/samples\/0 must have required property 'log_likelihood'/],
  ['piecewise', 'an infinite log-likelihood', r => change(r, draw(0), { log_likelihood: -Infinity }), /\/samples\/0\/log_likelihood must be number/],
  ['gp', 'a posterior function without its log-likelihood', r => omit(r, draw(2), 'log_likelihood'), /\/samples\/2 must have required property 'log_likelihood'/],
  ['wiener', 'a posterior function without its log-likelihood', r => omit(r, draw(1), 'log_likelihood'), /\/samples\/1 must have required property 'log_likelihood'/],
  ['wiener', 'a log-likelihood that is not a number', r => change(r, draw(1), { log_likelihood: NaN }), /\/samples\/1\/log_likelihood must be number/],
  ['program', 'a program without its log-likelihood', r => omit(r, record => record.kind === 'expression' && record.program === 1, 'log_likelihood'), /\/expressions\/1 must have required property 'log_likelihood'/],
  ['piecewise', 'line pieces inconsistent with the curve', r => change(r, draw(0), { values: range(31, () => 0) }), /posterior function/],
  ['gp', 'a different number of functions from the request', r => r, /Expected 4 posterior functions, got 3/, { count: 4 }],
  ['wiener', 'fewer functions than the request', r => r.slice(0, -2).concat(timing), /Expected 3 posterior functions, got 2/],
  ['clustering', 'two centres for one cluster', r => change(r, record => record.point === 3, { sigma: 2 }), /two different centres/],
  ['clustering', 'a zero spread', r => change(r, record => record.point === 3, { sigma: 0 }), /\/samples\/3\/sigma/],
  ['irm', 'a probability above 1', r => change(r, record => record.person === 2, { probabilities: [1, 0, 1.2, 0, 0, 0] }), /\/samples\/2\/probabilities\/2/],
  ['irm', 'a row of five probabilities', r => change(r, record => record.person === 2, { probabilities: [1, 0, 0, 0, 0] }), /\/samples\/2\/probabilities/],
  ['program', 'a program without sample points', r => without(r, record => record.kind === 'sample' && record.program === 1), /do not match/],
  ['program', 'programs that do not start at 0', r => r.map(record => (record.program === undefined ? record : { ...record, program: record.program + 1 })), /do not match/],
  ['program', 'a different number of programs from the request', r => r, /Expected 3 posterior programs, got 2/, { count: 3 }],
  ['ibp', 'feature rows of different lengths', r => change(r, record => record.country === 4, { features: [1, 0] }), /differ in length/],
  ['ibp', 'a feature value of 2', r => change(r, record => record.country === 4, { features: [1, 2, 1] }), /\/samples\/4\/features\/1/],
  ['graph', 'an adjacency matrix that is not symmetric', r => change(r, record => record.vertex === 2, { edges: flip(range(15, o => (2 + o) % 2), 6) }), /not symmetric/],
  ['graph', 'more matches than vertex pairs', r => change(r, record => record.kind === 'graph_stat', { matches: 106 }), /More matching vertex pairs/],
  ['graph', 'a row of fourteen edges', r => change(r, record => record.vertex === 2, { edges: range(14, () => 0) }), /\/samples\/2\/edges/],
  ['mondrian', 'overlapping half-square blocks', r => change(r, record => record.block === 1, { y_lower: 0, y_upper: 0.5 }), /overlap/],
  ['mondrian', 'duplicate block identifiers', r => change(r, record => record.block === 1, { block: 0 }), /identifiers/],
  ['mondrian', 'blocks that cover half the square', r => without(r, record => record.block === 1), /do not tile/],
  ['mondrian', 'a block with no width', r => [{ kind: 'sample', block: 2, x_lower: 0.5, x_upper: 0.5, y_lower: 0, y_upper: 0.5, value: 0.1 }, ...r], /no area/],
  ['mondrian', 'a renamed field', r => rename(r, record => record.block === 0, 'x_lower', 'x_lower_renamed'), /\/samples\/0 must have required property 'x_lower'/],
];

const GROUP_SIZES = {
  poisson: { samples: 2 },
  regression: { observations: 7, samples: 1000 },
  piecewise: { observations: 7, samples: 1 },
  gp: { observations: 7, samples: 3 },
  wiener: { observations: 7, samples: 3 },
  clustering: { samples: 10 },
  irm: { samples: 6 },
  program: { observations: 7, expressions: 2, samples: 6 },
  ibp: { samples: 16 },
  graph: { samples: 15 },
  mondrian: { samples: 2 },
};

// The page imports protocol.js and validators.js as plain ES modules, so the tests do the same, in the browser.
test.describe('decode', () => {
  let page;
  test.beforeAll(async ({ browser }, testInfo) => {
    page = await browser.newPage({ baseURL: testInfo.project.use.baseURL });
    await page.goto('/index.html');
  });
  test.afterAll(async () => page.close());

  const decode = (kind, request, records) =>
    page.evaluate(
      async ([kind, request, records]) => {
        const { decode } = await import('/protocol.js');
        try {
          const result = decode(kind, request, records);
          return {
            sizes: [result.observations.length, result.expressions.length, result.samples.length],
            elapsed: result.timing.elapsed_ms,
            graphStat: result.graphStat,
          };
        } catch (error) {
          return { error: error.message };
        }
      },
      [kind, request, records]
    );

  for (const kind of KINDS) {
    test(`${kind}: accepts a complete result and groups it by record kind`, async () => {
      const { observations = 0, expressions = 0, samples } = GROUP_SIZES[kind];
      const result = await decode(kind, requestFor(kind), COMPLETE[kind]());
      expect(result).toMatchObject({ sizes: [observations, expressions, samples], elapsed: 12.5 });
    });

    test(`${kind}: rejects a result that holds only its timing record`, async () => {
      const { error } = await decode(kind, requestFor(kind), [timing]);
      expect(error).toMatch(/result must have required property/);
    });

    test(`${kind}: rejects a result without sample records`, async () => {
      const records = without(COMPLETE[kind](), record => record.kind === 'sample');
      const { error } = await decode(kind, requestFor(kind), records);
      expect(error).toMatch(/result must have required property 'samples'/);
    });

    test(`${kind}: rejects a result without a timing record`, async () => {
      const records = without(COMPLETE[kind](), record => record.kind === 'timing');
      const { error } = await decode(kind, requestFor(kind), records);
      expect(error).toMatch(/result must have required property 'timings'/);
    });
  }

  for (const [kind, name, mutate, error, request] of REJECTED) {
    test(`${kind}: rejects ${name}`, async () => {
      const result = await decode(kind, request ?? requestFor(kind), mutate(COMPLETE[kind]()));
      expect(result.error).toMatch(error);
    });
  }

  test('mondrian: accepts unique identifiers independently of record order', async () => {
    // The renderer uses each rectangle's coordinates, not its identifier or array index.
    const records = COMPLETE.mondrian().map(record => record.block === undefined
      ? record : { ...record, block: record.block === 0 ? 17 : 4 });
    expect(await decode('mondrian', {}, records)).toMatchObject({ sizes: [0, 0, 2] });
  });

  test('rejects an unknown demo and records that are not an array', async () => {
    expect((await decode('toString', {}, [timing])).error).toMatch(/Unknown model/);
    expect((await decode('poisson', {}, { kind: 'timing' })).error).toMatch(/Expected records array/);
  });

  test('reports the graph statistic of a complete graph result', async () => {
    const { graphStat } = await decode('graph', {}, COMPLETE.graph());
    expect([graphStat.matches, graphStat.theta]).toEqual([96, 0.6]);
  });
});

test('validators.js is what test/protocol.schema.json generates', () => {
  const committed = readFileSync(validatorsUrl, 'utf8');
  expect(committed === generate(), 'run `npm run validators` in web/playground/test and commit validators.js').toBe(true);
});
