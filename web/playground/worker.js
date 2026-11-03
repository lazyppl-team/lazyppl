import {WASI, File, OpenFile, ConsoleStdout} from '/wasi/index.js';

let compiled;
const compile = () => compiled ??= WebAssembly.compileStreaming(fetch('./playground.wasm')).catch(error => {
  compiled = undefined;
  throw error;
});
self.onmessage = async ({data}) => {
  try {
    const module = await compile();
    const stdout = [];
    const stderr = [];
    const wasi = new WASI(['playground', ...data.args], [], [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered(line => stdout.push(line)),
      ConsoleStdout.lineBuffered(line => stderr.push(line)),
    ]);
    const start = performance.now();
    const instance = await WebAssembly.instantiate(module, {wasi_snapshot_preview1: wasi.wasiImport});
    const exitCode = wasi.start(instance);
    if (exitCode !== 0) throw new Error(`Exit ${exitCode}: ${stderr.join('\n')}`);
    postMessage({kind:data.kind, records:stdout.map(JSON.parse), stderr, runMs:performance.now()-start});
  } catch(error) {
    postMessage({kind:data.kind, error:String(error)});
  }
};
