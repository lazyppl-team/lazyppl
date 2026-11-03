import { defineConfig } from '@playwright/test';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const testDir = path.dirname(fileURLToPath(import.meta.url));
const playgroundRoot = process.env.LAZYPPL_PLAYGROUND_ROOT ?? path.resolve(testDir, '../dist-playground');
const port = 4173;

export default defineConfig({
  testDir,
  testMatch: '**/*.spec.mjs',
  outputDir: '/tmp/lazyppl-playwright-results',
  timeout: 60000,
  use: {
    baseURL: `http://127.0.0.1:${port}`,
    headless: true,
  },
  projects: [
    {
      name: 'chromium',
      use: { browserName: 'chromium' },
    },
  ],
  webServer: {
    command: `python3 -m http.server ${port} --bind 127.0.0.1`,
    cwd: playgroundRoot,
    url: `http://127.0.0.1:${port}/index.html`,
    reuseExistingServer: !process.env.CI,
  },
});
