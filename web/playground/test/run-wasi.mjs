import {readFileSync} from 'node:fs';
import {WASI} from 'node:wasi';
const binary=process.argv[2];
const wasi=new WASI({version:'preview1',args:[binary,...process.argv.slice(3)],returnOnExit:true});
const {instance}=await WebAssembly.instantiate(readFileSync(binary),{wasi_snapshot_preview1:wasi.wasiImport});
process.exitCode=wasi.start(instance);
