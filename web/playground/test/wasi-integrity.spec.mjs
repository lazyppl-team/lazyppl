import { test, expect } from '@playwright/test';
import { cp, readFile, writeFile } from 'node:fs/promises';
import { spawnSync } from 'node:child_process';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const checker = fileURLToPath(new URL('../../wasi/check-integrity.mjs', import.meta.url));

test('a changed WASI byte fails the integrity check', async ({}, testInfo) => {
  const directory = testInfo.outputPath('wasi');
  await cp(dirname(checker), directory, { recursive: true });
  const check = () => spawnSync(process.execPath, [checker, directory], { encoding: 'utf8' });
  const original = check();
  expect(original.status, original.stderr).toBe(0);

  const file = join(directory, 'index.js');
  const bytes = await readFile(file);
  bytes[0] ^= 1;
  await writeFile(file, bytes);
  const changed = check();
  expect(changed.status).not.toBe(0);
  expect(changed.stderr).toContain('index.js');
});
