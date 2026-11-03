import { createHash } from 'node:crypto';
import { readFileSync, readdirSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export function checkWasi(directory) {
  const { files } = JSON.parse(readFileSync(join(directory, 'vendor.json')));
  if (!files || typeof files !== 'object' || Array.isArray(files)) throw Error('Missing WASI file hashes');
  const expected = Object.keys(files).sort();
  const actual = readdirSync(directory).filter(name => name.endsWith('.js')).sort();
  if (!expected.length || JSON.stringify(expected) !== JSON.stringify(actual)) {
    throw Error(`WASI file set differs: expected ${expected}; got ${actual}`);
  }
  for (const name of expected) {
    if (!/^[a-z_]+\.js$/.test(name) || !/^[0-9a-f]{64}$/.test(files[name])) {
      throw Error(`Invalid WASI file or hash: ${name}`);
    }
    const digest = createHash('sha256').update(readFileSync(join(directory, name))).digest('hex');
    if (digest !== files[name]) throw Error(`WASI hash mismatch: ${name}`);
  }
  console.log(`Verified ${expected.length} WASI files in ${directory}`);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  checkWasi(process.argv[2] ?? dirname(fileURLToPath(import.meta.url)));
}
