import { decode } from './protocol.js';
const X_END = 6;
const gridX = (i, positions) => (i * X_END) / (positions - 1);
const quantileIndex = (n, permille) => Math.ceil((n * permille) / 1000) - 1;
const formatCount = n => n.toLocaleString('en-GB');
let worker;
let busy = false;
const seed = document.querySelector('#seed');
const buttons = [...document.querySelectorAll('button[id$="-run"]')];
const setBusy = value => {
  busy = value;
  for (const button of [...buttons, ...document.querySelectorAll('input')]) button.disabled = value;
  for (const button of document.querySelectorAll('[data-cancel],#cancel')) button.disabled = !value;
};

function applySettings(settings) {
  if (!settings || typeof settings !== 'object') return;
  for (const [kind, info] of Object.entries(settings)) {
    if (!info || info.min === undefined) continue;
    const input = document.querySelector(kind === 'regression' ? '#noise' : `#${kind}-count`);
    if (input) {
      input.min = String(info.min);
      input.max = String(info.max);
      input.step = String(info.step);
    }
  }
}

function createWorker() {
  const currentWorker = new Worker('./worker.js', { type: 'module' });
  worker = currentWorker;
  currentWorker.onerror = event => {
    currentWorker.terminate();
    if (worker === currentWorker) worker = null;
    finishError(event.message || 'Worker failed to load.');
  };
  currentWorker.onmessage = ({ data }) => {
    if (data.kind === 'settings') {
      if (!data.error) {
        applySettings(data.settings ?? (Array.isArray(data.records) ? data.records[0] : null));
      }
      return;
    }
    if (data.error) return finishError(data.error);
    try {
      const output = document.querySelector(`#${data.kind}-result`);
      const { observations, samples: rows, expressions, graphStat, timing } = decode(
        data.kind,
        { count: Number(output.dataset.count) },
        data.records
      );
      const points = observations.map(row => [row.x, row.y]);
      document.querySelector(`#${data.kind}-data`).textContent = JSON.stringify(rows);
      const elapsed = timing.elapsed_ms;
      if (data.kind === 'regression') {
        drawRegression(rows, points);
        output.textContent = `${formatCount(rows.length)} posterior functions, observation noise ${output.dataset.noise}.\n${Math.round(elapsed)} ms.`;
      } else if (['gp', 'wiener'].includes(data.kind)) {
        drawCurves(data.kind, rows, points);
        output.textContent = `${rows.length} posterior functions at ${rows[0].values.length} positions.\n10,000 burn-in steps; one retained function per 1,000 steps.\n${Math.round(elapsed)} ms.`;
      } else if (data.kind === 'clustering') {
        drawClusters(rows);
        const clusterCount = new Set(rows.map(row => row.cluster)).size;
        output.textContent = `Highest-scoring assignment from ${output.dataset.count} inference steps.\n${clusterCount} ${clusterCount === 1 ? 'cluster' : 'clusters'}. ${Math.round(elapsed)} ms.`;
      } else if (['irm', 'ibp'].includes(data.kind)) {
        const labels = rows.map(row => row.label);
        const columns =
          data.kind === 'irm' ? labels : Array.from({ length: rows[0].features.length }, (_, i) => String(i + 1));
        drawMatrix(data.kind, rows, labels, columns);
        output.textContent =
          data.kind === 'irm'
            ? `Pairwise group probabilities from ${output.dataset.count} inference steps.\n${Math.round(elapsed)} ms.`
            : `${columns.length} inferred features in the highest-scoring assignment from ${output.dataset.count} steps.\n${Math.round(elapsed)} ms.`;
      } else if (data.kind === 'program') {
        drawPrograms(rows, points);
        output.textContent = `${expressions.length} posterior programs, with 1,000 burn-in steps and 1,000 steps between programs.\n${Math.round(elapsed)} ms. Expressions (rounded constants):\n${expressions.map((row, i) => `${i + 1}. ${row.expression}`).join('\n')}`;
      } else if (data.kind === 'graph') {
        drawGraph(rows);
        output.textContent = `Final draw after ${output.dataset.count} steps.\n${graphStat.matches} of ${(rows.length * (rows.length - 1)) / 2} vertex pairs match the observed graph.\nDistance parameter ${Number(graphStat.theta).toFixed(3)} radians. ${Math.round(elapsed)} ms.`;
      } else if (data.kind === 'mondrian') {
        drawMondrian(rows);
        output.textContent = `${rows.length} blocks at lifetime ${output.dataset.count}.\n${Math.round(elapsed)} ms.`;
      } else {
        output.textContent =
          'k\tpoisson(k)\n--\t----------\n' +
          rows.map(row => `${row.count}\t${row.frequency}`).join('\n') +
          `\n10,000 samples in ${Math.round(elapsed)} ms.`;
      }
      output.dataset.runMs = data.runMs;
      output.dataset.kernelMs = elapsed;
      output.dataset.finishedMs = performance.now();
      output.dataset.state = 'done';
      setBusy(false);
    } catch (error) {
      finishError(error.message ?? String(error));
    }
  };
  currentWorker.postMessage({ kind: 'settings', args: ['--settings'] });
}

function finishError(message) {
  for (const output of document.querySelectorAll('pre[data-state="running"]')) {
    output.textContent = `Could not run: ${message}`;
    output.dataset.state = 'error';
  }
  setBusy(false);
}

function finishCancel() {
  for (const output of document.querySelectorAll('pre[data-state="running"]')) {
    output.textContent = 'Computation cancelled.';
    output.dataset.state = 'cancelled';
  }
  setBusy(false);
}

function run(kind) {
  if (busy || !seed.reportValidity()) return;
  const noise = document.querySelector('#noise');
  if (kind === 'regression' && !noise.reportValidity()) return;
  const count = document.querySelector(`#${kind}-count`);
  if (count && !count.reportValidity()) return;
  if (!worker) createWorker();
  const output = document.querySelector(`#${kind}-result`);
  output.textContent = 'Running…';
  output.dataset.state = 'running';
  output.dataset.noise = noise.value;
  if (count) output.dataset.count = count.value;
  setBusy(true);
  const args = [kind, seed.value];
  if (kind === 'regression') args.push(noise.value);
  else if (count) args.push(count.value);
  worker.postMessage({ kind, args });
}

function surface(kind) {
  const svg = document.querySelector(`#${kind}-plot`);
  for (const child of [...svg.children]) if (!['title', 'desc'].includes(child.tagName)) child.remove();
  return (name, attrs, text) => {
    const element = document.createElementNS('http://www.w3.org/2000/svg', name);
    for (const [key, value] of Object.entries(attrs)) element.setAttribute(key, value);
    if (text !== undefined) element.textContent = text;
    svg.append(element);
    return element;
  };
}

function drawCurves(kind, rows, observations) {
  const add = surface(kind);
  const values = rows.flatMap(row => row.values).concat(observations.map(row => row[1]));
  const lower = Math.min(-2, ...values) - 0.5,
    upper = Math.max(10, ...values) + 0.5;
  const x = value => 70 + (value / X_END) * 620,
    y = value => 350 - ((value - lower) / (upper - lower)) * 315;
  for (const row of rows)
    add('path', {
      class: 'function-draw',
      d: row.values.map((value, i) => `${i ? 'L' : 'M'}${x(gridX(i, row.values.length))} ${y(value)}`).join(' '),
    });
  add('path', { class: 'axis', d: 'M70 25V350H700', fill: 'none' });
  for (const value of [0, X_END / 2, X_END])
    add('text', { class: 'axis-label', x: x(value), y: 385, 'text-anchor': 'middle' }, String(value));
  for (const value of [0, 5, 10])
    add('text', { class: 'axis-label', x: 56, y: y(value) + 8, 'text-anchor': 'end' }, String(value));
  for (const [a, b] of observations) add('circle', { class: 'observation', cx: x(a), cy: y(b), r: 5 });
  add('text', { class: 'axis-label', x: 380, y: 416, 'text-anchor': 'middle' }, 'x');
  document.querySelector(`#${kind}-description`).textContent =
    `${rows.length} posterior functions and ${observations.length} observations, on a grid of ${rows[0].values.length} positions from 0 to ${X_END}.`;
}

function drawClusters(rows) {
  const add = surface('clustering');
  const groups = [...new Set(rows.map(row => row.cluster))];
  const x = value => 70 + (value / 10) * 620,
    y = value => 350 - (value / 10) * 315;
  const colour = value => `hsl(${value * 365} 75% 45%)`;
  for (const group of groups) {
    const row = rows.find(r => r.cluster === group);
    add('ellipse', {
      cx: x(row.centre_x),
      cy: y(row.centre_y),
      rx: ((row.sigma * 2) / 10) * 620,
      ry: ((row.sigma * 2) / 10) * 315,
      fill: 'none',
      stroke: colour(group),
      'stroke-width': 1.5,
    });
  }
  add('path', { class: 'axis', d: 'M70 25V350H700', fill: 'none' });
  for (const value of [0, 5, 10]) {
    add('text', { class: 'axis-label', x: x(value), y: 385, 'text-anchor': 'middle' }, String(value));
    add('text', { class: 'axis-label', x: 56, y: y(value) + 8, 'text-anchor': 'end' }, String(value));
  }
  for (const row of rows) {
    add('circle', { cx: x(row.x), cy: y(row.y), r: 5, fill: colour(row.cluster) });
    add('text', { class: 'point-label', x: x(row.x) + 8, y: y(row.y) - 8 }, String(groups.indexOf(row.cluster) + 1));
  }
  document.querySelector('#clustering-description').textContent =
    `${rows.length} observations assigned to ${groups.length} clusters, numbered by cluster.`;
}

function drawMatrix(kind, rows, rowLabels, columnLabels) {
  const add = surface(kind),
    width = 550 / columnLabels.length,
    height = 310 / rows.length;
  if (columnLabels.length === 0)
    add('text', { class: 'axis-label', x: 360, y: 210, 'text-anchor': 'middle' }, 'No features in this sample');
  for (const [i, row] of rows.entries()) {
    const values = kind === 'irm' ? row.probabilities : row.features;
    add('text', { class: 'point-label', x: 120, y: 60 + (i + 0.65) * height, 'text-anchor': 'end' }, rowLabels[i]);
    for (const [j, value] of values.entries()) {
      add('rect', {
        x: 130 + j * width,
        y: 60 + i * height,
        width: width - 1,
        height: height - 1,
        fill: 'var(--accent)',
        'fill-opacity': 0.05 + 0.95 * value,
      });
      add(
        'text',
        {
          class: 'point-label',
          x: 130 + (j + 0.5) * width,
          y: 60 + (i + 0.65) * height,
          'text-anchor': 'middle',
          style: `fill:${value > 0.5 ? 'var(--button-ink)' : 'var(--ink)'}`,
        },
        kind === 'ibp' ? String(value) : value.toFixed(2)
      );
    }
  }
  for (const [j, label] of columnLabels.entries())
    add('text', { class: 'point-label', x: 130 + (j + 0.5) * width, y: 45, 'text-anchor': 'middle' }, label);
  document.querySelector(`#${kind}-description`).textContent =
    kind === 'irm'
      ? `${rowLabels.join(', ')}. Rows and columns identify people; cell values are probabilities of sharing a group.`
      : `${rowLabels.join(', ')}. Rows identify countries and columns identify features. A one indicates membership.`;
}

function drawPrograms(rows, observations) {
  const add = surface('program');
  const lower = Math.min(-2, ...rows.map(row => row.y)) - 0.5,
    upper = Math.max(10, ...rows.map(row => row.y)) + 0.5;
  const x = value => 70 + ((value + 0.25) / 6.45) * 620,
    y = value => 350 - ((value - lower) / (upper - lower)) * 315;
  for (const i of new Set(rows.map(row => row.program))) {
    add('path', {
      class: 'function-draw',
      style: `stroke:hsl(${i * 137.5} 70% 45%);stroke-opacity:1`,
      d: rows
        .filter(row => row.program === i)
        .map((row, j) => `${j ? 'L' : 'M'}${x(row.x)} ${y(row.y)}`)
        .join(' '),
    });
  }
  add('path', { class: 'axis', d: 'M70 25V350H700', fill: 'none' });
  for (const value of [0, 3, 6])
    add('text', { class: 'axis-label', x: x(value), y: 385, 'text-anchor': 'middle' }, String(value));
  for (const value of [0, 5, 10])
    add('text', { class: 'axis-label', x: 56, y: y(value) + 8, 'text-anchor': 'end' }, String(value));
  for (const [a, b] of observations) add('circle', { class: 'observation', cx: x(a), cy: y(b), r: 5 });
  document.querySelector('#program-description').textContent =
    `${new Set(rows.map(row => row.program)).size} inferred programs and ${observations.length} observations.`;
}

function drawGraph(rows) {
  const add = surface('graph'),
    n = rows.length;
  const point = i => [
    360 + 155 * Math.cos((i / n) * 2 * Math.PI - Math.PI / 2),
    210 + 155 * Math.sin((i / n) * 2 * Math.PI - Math.PI / 2),
  ];
  for (let i = 0; i < n; i++)
    for (let j = i + 1; j < n; j++)
      if (rows[i].edges[j]) {
        const a = point(i),
          b = point(j);
        add('path', { class: 'axis', d: `M${a[0]} ${a[1]}L${b[0]} ${b[1]}`, 'stroke-opacity': 0.5 });
      }
  for (let i = 0; i < n; i++) {
    const [x, y] = point(i);
    add('circle', { cx: x, cy: y, r: 13, fill: 'var(--accent)' });
    add(
      'text',
      { class: 'point-label', x, y: y + 5, 'text-anchor': 'middle', style: 'fill:var(--button-ink)' },
      String(i + 1)
    );
  }
  document.querySelector('#graph-description').textContent =
    `A sampled graph on ${n} numbered vertices, in a circular layout.`;
}

function drawMondrian(rows) {
  const add = surface('mondrian'),
    x = value => 205 + 310 * value,
    y = value => 355 - 310 * value;
  for (const { x_lower: a, x_upper: b, y_lower: c, y_upper: d, value: p } of rows) {
    const rectangle = add('rect', {
      x: x(a),
      y: y(d),
      width: (b - a) * 310,
      height: (d - c) * 310,
      fill: 'var(--accent)',
      'fill-opacity': 0.1 + 0.9 * p,
      stroke: 'var(--border)',
      'stroke-width': 1,
    });
    const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
    title.textContent = `Value ${p.toFixed(3)}; x ${a.toFixed(3)} to ${b.toFixed(3)}, y ${c.toFixed(3)} to ${d.toFixed(3)}`;
    rectangle.append(title);
    if ((b - a) * 310 > 35 && (d - c) * 310 > 20)
      add(
        'text',
        {
          class: 'point-label',
          x: x((a + b) / 2),
          y: y((c + d) / 2) + 5,
          'text-anchor': 'middle',
          style: `fill:${p > 0.5 ? 'var(--button-ink)' : 'var(--ink)'}`,
        },
        p.toFixed(2)
      );
  }
  for (const value of [0, 0.5, 1]) {
    add('text', { class: 'point-label', x: x(value), y: 385, 'text-anchor': 'middle' }, String(value));
    add('text', { class: 'point-label', x: 190, y: y(value) + 5, 'text-anchor': 'end' }, String(value));
  }
  document.querySelector('#mondrian-description').textContent =
    `A partition of the unit square into ${rows.length} rectangular blocks, with independently sampled values.`;
}

function drawRegression(rows, observations) {
  const svg = document.querySelector('#regression-plot');
  for (const child of [...svg.children]) if (!['title', 'desc'].includes(child.tagName)) child.remove();
  const add = (name, attrs, text) => {
    const el = document.createElementNS('http://www.w3.org/2000/svg', name);
    for (const [key, value] of Object.entries(attrs)) el.setAttribute(key, value);
    if (text !== undefined) el.textContent = text;
    svg.append(el);
    return el;
  };
  const positions = rows[0].predictive.length;
  const bands = Array.from({ length: positions }, (_, i) => {
    const ys = rows.map(row => row.predictive[i]).sort((a, b) => a - b);
    return [gridX(i, positions), ys[quantileIndex(ys.length, 25)], ys[quantileIndex(ys.length, 975)]];
  });
  const lower = Math.min(-2, ...bands.map(b => b[1])) - 0.5;
  const upper = Math.max(10, ...bands.map(b => b[2])) + 0.5;
  const x = value => 70 + (value / X_END) * 620;
  const y = value => 350 - ((value - lower) / (upper - lower)) * 315;
  const points = [
    ...bands.map(b => `${x(b[0])},${y(b[1])}`),
    ...bands.toReversed().map(b => `${x(b[0])},${y(b[2])}`),
  ].join(' ');
  add('polygon', { class: 'predictive', points });
  for (const row of rows) add('path', { class: 'posterior', d: `M${x(0)} ${y(row.f0)}L${x(X_END)} ${y(row.f6)}` });
  add('path', { class: 'axis', d: 'M70 25V350H700', fill: 'none' });
  for (const value of [0, X_END / 2, X_END])
    add('text', { class: 'axis-label', x: x(value), y: 385, 'text-anchor': 'middle' }, String(value));
  for (const value of [0, 5, 10])
    if (value >= lower && value <= upper)
      add('text', { class: 'axis-label', x: 56, y: y(value) + 8, 'text-anchor': 'end' }, String(value));
  for (const [a, b] of observations) add('circle', { class: 'observation', cx: x(a), cy: y(b), r: 5 });
  add('text', { class: 'axis-label', x: 380, y: 416, 'text-anchor': 'middle' }, 'x');
  document.querySelector('#plot-description').textContent =
    `${observations.length} observations, ${formatCount(rows.length)} posterior functions and a 95% posterior predictive interval. Observation noise ${document.querySelector('#regression-result').dataset.noise}.`;
}
document.querySelector('#poisson-run').onclick = () => run('poisson');
document.querySelector('#regression-run').onclick = () => run('regression');
document.querySelector('#gp-run').onclick = () => run('gp');
document.querySelector('#wiener-run').onclick = () => run('wiener');
document.querySelector('#clustering-run').onclick = () => run('clustering');
document.querySelector('#irm-run').onclick = () => run('irm');
document.querySelector('#program-run').onclick = () => run('program');
document.querySelector('#ibp-run').onclick = () => run('ibp');
document.querySelector('#graph-run').onclick = () => run('graph');
document.querySelector('#mondrian-run').onclick = () => run('mondrian');
const cancelComputation = () => {
  if (worker) worker.terminate();
  worker = null;
  finishCancel();
  createWorker();
};
for (const button of document.querySelectorAll('[data-cancel],#cancel')) button.onclick = cancelComputation;
createWorker();
run('poisson');
