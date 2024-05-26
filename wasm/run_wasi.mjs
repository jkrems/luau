#!/usr/bin/env node

import { WASI } from "node:wasi";
import { resolve } from "node:path";
import { readFile } from "node:fs/promises";

const wasi = new WASI({
  version: "preview1",
  args: [],
  env: {},
});

const wasmPath = resolve(process.argv[2]);
const wasmModule = await WebAssembly.compile(await readFile(wasmPath));
const wasmInstance = await WebAssembly.instantiate(
  wasmModule,
  Object.assign({}, wasi.getImportObject(), {
    "luau:util": {
      print_number: (n) => console.log({ n }),
    },
  })
);

wasi.start(wasmInstance);
