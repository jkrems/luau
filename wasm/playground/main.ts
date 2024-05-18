import * as monaco from "monaco-editor";

import { LuauSyntaxError, isLuauSyntaxError, parse } from "../src/parse";
import { language as watLang, conf as watConf } from "./wat.monarch";

interface LuauWebModule {
  luauToWasm(code: string, optLevel: number, asWat: boolean): Uint8Array;
}

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

function main(luau: LuauWebModule) {
  registerLanguages();

  const lastSource = window.localStorage.getItem("lastSource") ?? "print(2);\n";

  const srcEditor = createEditor("src", "lua", lastSource);
  const srcDecorations = srcEditor.createDecorationsCollection();

  const astEditor = createEditor("ast", "json");

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

    // let ast;
    // try {
    //   ast = parse(code);
    //   astEditor.setValue(JSON.stringify(ast, null, 2));
    //   monaco.editor.setModelMarkers(srcEditor.getModel()!, "luau", []);
    // } catch (e) {
    //   if (isLuauSyntaxError(e)) {
    //     monaco.editor.setModelMarkers(srcEditor.getModel()!, "luau", [
    //       {
    //         message: `${e.message}`,
    //         severity: monaco.MarkerSeverity.Error,
    //         startLineNumber: e.location.start.line,
    //         startColumn: e.location.start.column,
    //         endLineNumber: e.location.end.line,
    //         endColumn: e.location.end.column,
    //       },
    //     ]);
    //     astEditor.setValue(
    //       `At input.lua:${e.location.start.line}:${e.location.start.column} (${e.location.start.offset})\n\n${e.message}`
    //     );
    //     return;
    //   } else {
    //     throw e;
    //   }
    // }

    try {
      const wat = luau.luauToWasm(code, optLevel, true);
      watEditor.setValue(new TextDecoder().decode(wat));
    } catch (e) {
      console.error(e);
      watEditor.setValue(`${e}`);
      if (isLuauSyntaxError(e)) {
        monaco.editor.setModelMarkers(srcEditor.getModel()!, "luau", [
          {
            message: `${e.message}`,
            severity: monaco.MarkerSeverity.Error,
            startLineNumber: e.location.start.line,
            startColumn: e.location.start.column,
            endLineNumber: e.location.end.line,
            endColumn: e.location.end.column,
          },
        ]);
      }
      return;
    }

    try {
      runEditor.setValue("<running>\n");

      const binaryOutput = luau.luauToWasm(code, optLevel, false);

      await WebAssembly.instantiate(binaryOutput, {
        "luau:util": {
          print: (value: unknown) => {
            runEditor.setValue(runEditor.getValue() + `print: ${value}\n`);
          },
        },
      });

      runEditor.setValue(runEditor.getValue() + `<exit>\n`);
    } catch (e) {
      runEditor.setValue(`${e}`);
      return;
    }
  }

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

function loadWasmCode(): LuauWebModule {
  return globalThis.Module;
}

Promise.all([loadWasmCode(), documentIsReady]).then(
  (args: [LuauWebModule, ...unknown[]]) => {
    main(args[0]);
  }
);
