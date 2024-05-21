import * as monaco from "monaco-editor";

import { LuauSyntaxError, isLuauSyntaxError, parse } from "../src/parse";
import { language as watLang, conf as watConf } from "./wat.monarch";
import { CompilationResult } from "./Luau.Web";

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

const DEFAULT_PROGRAM = `type Vector2d = {x: number, y: number}

local function vec2d(x: number, y: number)
    local v = {};
    v.x = x;
    v.y = y;
    return v;
end

local function vecLen(v: Vector2d)
    return (v.x * v.x + v.y * v.y) ^ 0.5;
end

local m: Vector2d = vec2d(3, 4);
print(vecLen(m));
`;

function main({ luauToWasm }: LuauWebModule) {
  registerLanguages();

  const lastSource =
    window.localStorage.getItem("lastSource") ?? DEFAULT_PROGRAM;

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
    } finally {
      result?.delete();
      result = null;
    }

    try {
      runEditor.setValue("<running>\n");

      result = luauToWasm(code, optLevel, false);
      const binaryOutput = result.getOutput();

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
    } finally {
      result?.delete();
      result = null;
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

async function loadWasmCode(): Promise<LuauWebModule> {
  const { default: loadLuauWeb } = await import("./Luau.Web");
  return loadLuauWeb();
}

Promise.all([loadWasmCode(), documentIsReady]).then(
  (args: [LuauWebModule, ...unknown[]]) => {
    main(args[0]);
  }
);
