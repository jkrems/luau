import * as monaco from "monaco-editor";

import { language as watLang, conf as watConf } from "./wat.monarch";
import { CompilationResult } from "./Luau.Web";

// @ts-ignore
import EXAMPLE_CALL from "../examples/call.lua?raw";
// @ts-ignore
import EXAMPLE_OPS from "../examples/ops.lua?raw";
// @ts-ignore
import EXAMPLE_VEC from "../examples/vec.lua?raw";
// @ts-ignore
import EXAMPLE_STR from "../examples/str.lua?raw";

const EXAMPLES = new Map<string, string>([
  ["call.lua", EXAMPLE_CALL],
  ["ops.lua", EXAMPLE_OPS],
  ["vec.lua", EXAMPLE_VEC],
  ["str.lua", EXAMPLE_STR],
]);

type LuauWebModule = Awaited<
  ReturnType<typeof import("./Luau.Web")["default"]>
>;

function createEditor(
  name: string,
  language: string,
  value = ""
): ReturnType<typeof monaco.editor.create> {
  const el = document.getElementById(`${name}-edit`)!;

  return monaco.editor.create(el, {
    value,
    language,
  });
}

function registerLanguages() {
  monaco.languages.register({ id: "wat" });
  monaco.languages.setLanguageConfiguration("wat", watConf);
  monaco.languages.setMonarchTokensProvider("wat", watLang);
}

function main({ luauToWasm }: LuauWebModule) {
  registerLanguages();

  const lastSource = window.localStorage.getItem("lastSource") ?? EXAMPLE_VEC;

  const srcEditor = createEditor("src", "lua", lastSource);
  const srcDecorations = srcEditor.createDecorationsCollection();

  const astEditor = createEditor("raw", "wat");

  const watEditor = createEditor("wat", "wat");

  const runEditor = createEditor("run", "text");

  const optLevelSelect: HTMLSelectElement = document.querySelector(
    'select[name="opt-level"]'
  )!;

  async function reparseSource() {
    const code = srcEditor.getValue();
    window.localStorage.setItem("lastSource", code);

    srcDecorations.clear();

    const optLevel = +optLevelSelect.value;

    let result: CompilationResult | null = null;

    try {
      result = luauToWasm(code, 0, true);
      astEditor.setValue(new TextDecoder().decode(result.getOutput()));
    } catch (e) {
      console.error(e);
      watEditor.setValue(`${e}`);
      return;
    } finally {
      result?.delete();
      result = null;
    }

    try {
      result = luauToWasm(code, optLevel, true);
      watEditor.setValue(new TextDecoder().decode(result.getOutput()));
    } catch (e) {
      console.error(e);
      watEditor.setValue(`${e}`);
      return;
    } finally {
      result?.delete();
      result = null;
    }

    try {
      runEditor.setValue("<running>\n");

      result = luauToWasm(code, optLevel, false);
      const binaryOutput = result.getOutput();

      let memory: WebAssembly.Memory;
      const { instance } = await WebAssembly.instantiate(binaryOutput, {
        "luau:util": {
          print: (value: unknown) => {
            runEditor.setValue(runEditor.getValue() + `print: ${value}\n`);
          },
        },
        wasi_snapshot_preview1: {
          fd_write: (fd: number, iovsPtr: number, iovsLen: number) => {
            if (fd === 1 /* stdout */ || fd === 2 /* stderr */) {
              const iovs = new Uint32Array(memory.buffer, iovsPtr, iovsLen * 2);
              for (let i = 0; i < iovsLen; ++i) {
                const iovPtr = iovs[i * 2 + 0];
                const iovLen = iovs[i * 2 + 1];
                const str = new TextDecoder().decode(
                  new Uint8Array(memory.buffer, iovPtr, iovLen)
                );
                runEditor.setValue(runEditor.getValue() + str);
              }
            } else {
              console.log({ fd, iovsPtr, iovsLen });
            }
          },
        },
      });
      if (instance.exports["memory"] instanceof WebAssembly.Memory) {
        memory = instance.exports["memory"];
      }
      const wasiMain = instance.exports["_start"];
      if (typeof wasiMain === "function") {
        wasiMain();
      }

      runEditor.setValue(runEditor.getValue() + `<exit>\n`);
    } catch (e) {
      runEditor.setValue(`${e}`);
      return;
    } finally {
      result?.delete();
      result = null;
    }
  }

  const exampleSelect = document.querySelector(
    'select[name="load-example'
  ) as HTMLSelectElement;
  for (const exampleName of EXAMPLES.keys()) {
    const opt = document.createElement("option");
    opt.value = exampleName;
    opt.textContent = exampleName;
    exampleSelect.appendChild(opt);
  }
  exampleSelect.addEventListener("change", () => {
    const exampleName = exampleSelect.value;
    if (exampleName) {
      srcEditor.setValue(EXAMPLES.get(exampleName)!);
    }
  });

  srcEditor.onDidChangeModelContent(reparseSource);
  queueMicrotask(reparseSource);
  optLevelSelect.addEventListener("change", reparseSource);
}

const documentIsReady = /^loaded|^i|^c/.test(document.readyState)
  ? Promise.resolve()
  : new Promise<unknown>((resolve) => {
      document.addEventListener("DOMContentLoaded", resolve, { once: true });
      window.addEventListener("load", resolve, { once: true });
    });

async function loadWasmCode(): Promise<LuauWebModule> {
  const { default: loadLuauWeb } = await import("./Luau.Web");
  return loadLuauWeb();
}

Promise.all([loadWasmCode(), documentIsReady]).then(
  (args: [LuauWebModule, ...unknown[]]) => {
    main(args[0]);
  }
);
