import * as validators from './validators.js';

const sum = values => values.reduce((a, b) => a + b, 0);
const need = (ok, message) => {
  if (!ok) throw new Error(message);
};

// Relations that a JSON Schema cannot state: between records, and between the result and the request.
const requestedSamples = ({ samples }, { count }) =>
  need(samples.length === count, `Expected ${count} posterior functions, got ${samples.length}`);

const INVARIANTS = {
  poisson: ({ samples, trace }) => {
    need(samples.every((row, i) => i === 0 || row.count > samples[i - 1].count), 'Poisson counts are not increasing');
    need(Math.abs(sum(samples.map(row => row.frequency)) - 1) < 1e-9, 'Poisson frequencies do not sum to 1');
    need(trace.count === trace.points.length, 'Poisson point count differs from the displayed arrivals');
    need(trace.points.every((x, i) => x > (trace.points[i - 1] ?? 0)), 'Poisson arrivals are not increasing');
    need(trace.addresses.length >= trace.count + 1 && trace.uniforms.length === trace.addresses.length,
      'Poisson traversal must include the first excluded arrival');
    need(new Set(trace.addresses.map(path => JSON.stringify(path))).size === trace.addresses.length,
      'Poisson traversal repeats a tree address');
    need(samples.some(row => row.count === trace.count), 'The traced count is absent from the histogram');
  },
  piecewise: (result, request) => {
    requestedSamples(result, request);
    for (const [i, row] of result.samples.entries()) {
      need(row.draw === i, 'Piecewise draws must start at zero and be consecutive');
      const bounds = [0, ...row.change_points, 6];
      need(row.pieces.length === bounds.length - 1 && row.pieces.every((p, j) =>
        p.x_lower === bounds[j] && p.x_upper === bounds[j + 1] && p.x_lower < p.x_upper),
      'Piecewise boundaries must tile [0,6] at the sampled change points');
      need(row.values.every((y, j) => {
        const x = j / 5;
        const p = row.pieces.find(p => x <= p.x_upper);
        const line = p.y_lower + (p.y_upper - p.y_lower) * (x - p.x_lower) / (p.x_upper - p.x_lower);
        return Math.abs(y - line) <= 1e-10 * Math.max(1, Math.abs(y));
      }), 'Line pieces differ from the posterior function');
    }
  },
  gp: requestedSamples,
  wiener: requestedSamples,
  clustering: ({ samples }) => {
    const centres = new Map();
    for (const row of samples) {
      const centre = `${row.centre_x} ${row.centre_y} ${row.sigma}`;
      if (!centres.has(row.cluster)) centres.set(row.cluster, centre);
      need(centres.get(row.cluster) === centre, `Cluster ${row.cluster} has two different centres`);
    }
  },
  program: ({ expressions, samples }, { count }) => {
    need(expressions.length === count, `Expected ${count} posterior programs, got ${expressions.length}`);
    const sampled = new Set(samples.map(row => row.program));
    need(
      expressions.every((row, i) => row.program === i && sampled.has(i)) && sampled.size === expressions.length,
      'The programs and their sample points do not match'
    );
  },
  ibp: ({ samples }) =>
    need(samples.every(row => row.features.length === samples[0].features.length), 'Feature rows differ in length'),
  graph: ({ graphStat, samples }) => {
    need(
      samples.every((row, i) => samples.every((other, j) => i === j || row.edges[j] === other.edges[i])),
      'The adjacency matrix is not symmetric'
    );
    need(graphStat.matches <= (samples.length * (samples.length - 1)) / 2, 'More matching vertex pairs than pairs');
  },
  mondrian: ({ samples }) => {
    need(
      samples.every(row => row.x_lower < row.x_upper && row.y_lower < row.y_upper),
      'A block has no area'
    );
    need(new Set(samples.map(row => row.block)).size === samples.length, 'Block identifiers must be unique');
    for (let i = 0; i < samples.length; i++) {
      const a = samples[i];
      for (const b of samples.slice(i + 1)) {
        need(
          Math.max(a.x_lower, b.x_lower) >= Math.min(a.x_upper, b.x_upper) ||
          Math.max(a.y_lower, b.y_lower) >= Math.min(a.y_upper, b.y_upper),
          `Blocks ${a.block} and ${b.block} overlap`
        );
      }
    }
    const area = sum(samples.map(row => (row.x_upper - row.x_lower) * (row.y_upper - row.y_lower)));
    need(Math.abs(area - 1) < 1e-9, 'The blocks do not tile the unit square');
  },
};

function describe({ instancePath, message, params }) {
  const extra = params.additionalProperty ? ` '${params.additionalProperty}'` : '';
  return `${instancePath || 'result'} ${message}${extra}`;
}

// Checks the JSON-line records of one run and returns them grouped by record kind. Throws on a result
// that is not the one hs/Main.hs prints for this demo and request, so that nothing is drawn from it.
export function decode(kind, request, records) {
  if (!Object.hasOwn(validators, kind)) throw new Error(`Unknown model: ${kind}`);
  if (!Array.isArray(records)) throw new Error(`Expected records array for model '${kind}'`);
  const groups = {};
  for (const record of records) (groups[`${record?.kind}s`] ??= []).push(record);
  const validate = validators[kind];
  if (!validate(groups)) throw new Error(`Invalid '${kind}' result: ${describe(validate.errors[0])}`);
  const result = {
    observations: groups.observations ?? [],
    samples: groups.samples,
    expressions: groups.expressions ?? [],
    graphStat: groups.graph_stats?.[0],
    trace: groups.traces?.[0],
    timing: groups.timings[0],
  };
  INVARIANTS[kind]?.(result, request);
  return result;
}
