import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { closeSync, mkdtempSync, openSync, readFileSync, readdirSync, rmSync, writeFileSync, mkdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

// GHC's own literate-Haskell and CPP passes determine which model code is built.
const root = resolve(fileURLToPath(new URL('../..', import.meta.url)));
const models = {
  poisson: 'examples/models/MinimalDemo.lhs',
  regression: 'examples/models/RegressionDemo.lhs',
  gp: 'examples/models/GaussianProcessDemo.lhs',
  wiener: 'examples/models/WienerDemo.lhs',
  clustering: 'examples/models/ClusteringDemo.lhs',
  irm: 'examples/models/IrmDemo.lhs',
  program: 'examples/models/ProgramInductionDemo.lhs',
  ibp: 'examples/models/AdditiveClusteringDemo.lhs',
  graph: 'examples/models/GraphDemo.lhs',
  mondrian: 'src/LazyPPL/Distributions/Mondrian.hs',
  controlflow: 'examples/models/ControlFlowDemo.lhs',
};
const driverImports = {
  gp: ['import Data.List (intercalate)'],
  regression: ['import Data.List (intercalate)'],
  irm: ['import Control.Monad (forM_)'],
  program: ['import Data.List (intercalate)'],
  mondrian: ['import Control.Monad (forM_)', 'import Data.List (intercalate)'],
};
const seedHelper = `seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"
`;
const [mode, directory, revision] = process.argv.slice(2);
if (!['generate', 'check'].includes(mode) || !directory || !/^[0-9a-f]{40}$/.test(revision ?? '')) {
  throw Error('Usage: node web/editor/generate-examples.mjs generate|check DIRECTORY SOURCE_REVISION');
}
const target = resolve(directory);
// The physics page has no browser runner. Keep its original site listing separately.
const physicsRevision = 'cc1b28932ef1c41bdf23cd16f23e7a4f44caeeb5';
const physicsSha256 = '5859df57da3023719896e27d8409db6a9823d71cbf7d76ea7094d72eabf139d0';
const paths = [...new Set([...Object.values(models), ...Object.keys(models).map(name => `web/editor/example-drivers/${name}.hs`),
  'web/editor/example-drivers/curves.hs'])];
const scratch = mkdtempSync(join(tmpdir(), 'lazyppl-examples-'));
const expected = new Map();
const inputs = {};
try {
  const archivePath = process.env.SOURCE_ARCHIVE ? resolve(process.env.SOURCE_ARCHIVE) : join(scratch, 'source.tar');
  if (!process.env.SOURCE_ARCHIVE) {
    writeFileSync(archivePath, execFileSync('git', ['archive', '--format=tar', revision, '--', ...paths],
      { cwd: root, maxBuffer: 16 * 1024 * 1024 }));
  }
  const fd = openSync(archivePath, 'r');
  let archiveRevision;
  try {
    archiveRevision = execFileSync('git', ['get-tar-commit-id'], { stdio: [fd, 'pipe', 'pipe'], encoding: 'utf8' }).trim();
  } finally {
    closeSync(fd);
  }
  if (archiveRevision !== revision) throw Error(`Source archive is ${archiveRevision}, not ${revision}`);
  const fromArchive = path => execFileSync('tar', ['-xOf', archivePath, path], { maxBuffer: 16 * 1024 * 1024 });
  for (const [name, input] of Object.entries(models)) {
    const output = join(scratch, `${name}.hs`);
    const source = fromArchive(input);
    mkdirSync(dirname(join(scratch, input)), { recursive: true });
    writeFileSync(join(scratch, input), source);
    execFileSync('ghc', ['-E', '-cpp', '-optP-P', input, '-o', output], { cwd: scratch, stdio: 'inherit' });
    let text = readFileSync(output, 'utf8');
    // The code is already preprocessed; the in-browser interpreter has no C preprocessor.
    text = text.replace(/^[ \t]*\{-# LANGUAGE CPP #-\}[ \t]*$/gm, '');
    let modules = [...text.matchAll(/^[ \t]*module[ \t]+\S+[ \t]+where[ \t]*$/gm)];
    if (modules.length !== 1) throw Error(`${input}: expected one module declaration`);
    // Bird-style literate source has a common margin left by GHC's unlit pass.
    const margin = modules[0][0].match(/^[ \t]*/)[0];
    if (margin) {
      text = text.split('\n').map(line => line.startsWith(margin) ? line.slice(margin.length) : line).join('\n');
      modules = [...text.matchAll(/^module[ \t]+\S+[ \t]+where[ \t]*$/gm)];
      if (modules.length !== 1) throw Error(`${input}: cannot remove literate margin`);
    }
    const randomImports = name === 'regression' ? 'mkStdGen,setStdGen,splitGen,StdGen'
      : name === 'mondrian' ? 'mkStdGen' : 'mkStdGen,setStdGen';
    const imports = ['import System.Environment (getArgs)', `import System.Random (${randomImports})`,
      'import Text.Read (readMaybe)', ...(driverImports[name] ?? [])].join('\n');
    text = text.replace(modules[0][0], `module Main where\n${imports}`);
    if (name === 'poisson') {
      if (!/^main :: IO \(\)$/m.test(text) || !/^main = /m.test(text)) throw Error(`${input}: missing original main`);
      text = text.replace(/^main :: IO \(\)$/m, 'demoMain :: IO ()').replace(/^main = /m, 'demoMain = ');
    }
    // GHC preserves prose as blank lines and emits line directives for diagnostics.
    text = text.replace(/^[ \t]*\{-# LINE \d+ "[^"\n]*" #-\}[ \t]*\n?/gm, '')
      .replace(/\n(?:[ \t]*\n){2,}/g, '\n\n').trim();
    const driverPath = `web/editor/example-drivers/${name}.hs`;
    const driverPaths = ['gp', 'wiener'].includes(name)
      ? ['web/editor/example-drivers/curves.hs', driverPath] : [driverPath];
    const driverBytes = driverPaths.map(fromArchive);
    const driver = driverBytes.map(bytes => bytes.toString('utf8')).join('\n');
    inputs[input] = createHash('sha256').update(source).digest('hex');
    driverPaths.forEach((path, index) => { inputs[path] = createHash('sha256').update(driverBytes[index]).digest('hex'); });
    const seedNote = '-- Pass the playground Random seed as the program argument; the default is 42.\n';
    expected.set(`${name}.hs`, `-- Generated from ${input} at ${revision}.\n${seedNote}${text.trimEnd()}\n\n${seedHelper}\n${driver}`);
  }
  const physicsPage = execFileSync('curl', ['-fsSL', `https://raw.githubusercontent.com/lazyppl-team/lazyppl-team.github.io/${physicsRevision}/Physics.html`]);
  if (createHash('sha256').update(physicsPage).digest('hex') !== physicsSha256) throw Error('Physics page differs from its pinned revision');
  const physicsListing = execFileSync('python3', [join(root, 'web/editor/extract-physics.py')], { input: physicsPage, encoding: 'utf8' });
  const physicsListingHash = createHash('sha256').update(physicsListing).digest('hex');
  const hashes = Object.fromEntries([...expected].map(([name, text]) => [name, createHash('sha256').update(text).digest('hex')]));
  expected.set('physics-listing.json', physicsListing);
  expected.set('examples.json', JSON.stringify({ source_revision: revision, physics_source_revision: physicsRevision, physics_listing_sha256: physicsListingHash, inputs, examples: hashes }, null, 2) + '\n');
  if (mode === 'generate') {
    mkdirSync(target, { recursive: true });
    const existing = readdirSync(target).filter(name => name.endsWith('.hs') || name === 'examples.json' || name === 'physics-listing.json');
    if (existing.length) throw Error(`Refusing to overwrite existing generated examples: ${existing.join(', ')}`);
    for (const [name, text] of expected) writeFileSync(join(target, name), text);
  } else {
    const actual = readdirSync(target).filter(name => name.endsWith('.hs') || name === 'examples.json' || name === 'physics-listing.json').sort();
    const names = [...expected.keys()].sort();
    if (JSON.stringify(actual) !== JSON.stringify(names)) throw Error(`Example set differs: expected ${names}; got ${actual}`);
    for (const name of Object.keys(models)) {
      const text = readFileSync(join(target, `${name}.hs`), 'utf8');
      const imports = [...text.matchAll(/^import\s+(?:qualified\s+)?([\w.]+)/gm)].map(match => match[1]);
      if (new Set(imports).size !== imports.length) throw Error(`${name} repeats a module import`);
    }
    const blocks = JSON.parse(readFileSync(join(target, 'physics-listing.json'), 'utf8'));
    if (!Array.isArray(blocks) || blocks.length !== 12) throw Error('Physics listing must contain 12 blocks');
    if (typeof blocks[1] !== 'string' || !blocks[1].startsWith('makeWorld "World"')) {
      throw Error('Physics listing second block must start with makeWorld "World"');
    }
    for (const [name, text] of expected) {
      if (readFileSync(join(target, name), 'utf8') !== text) throw Error(`${basename(target)}/${name} differs from ${revision}`);
    }
    console.log(`Verified ${expected.size - 2} editor examples against ${revision} and the pinned physics listing`);
  }
} finally {
  rmSync(scratch, { recursive: true });
}
