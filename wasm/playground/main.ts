import * as monaco from "monaco-editor";

import * as LuauParser from "../src/luau.pegjs";
import { language as watLang, conf as watConf } from "./wat.monarch";

async function loadMonaco() {}

interface CodeLocation {
  offset: number;
  line: number;
  column: number;
}

interface PegSyntaxError extends Error {
  location: {
    start: CodeLocation;
    end: CodeLocation;
  };
}

function isSyntaxError(e: unknown): e is PegSyntaxError {
  return e instanceof LuauParser.SyntaxError;
}

function toWat(ast: any): string {
  const types: string[] = [];
  let funcs: string[] = [];
  let instructions: string[] = [];
  let locals: string[] = [];
  let localTypes = new Map<string, any>();

  function toWasmTypeRef(type: any): string {
    switch (type.type) {
      case "TypeRef": {
        switch (type.base) {
          case "number":
            return "f64";
          default:
            return `(ref null 0 (; $${type.base} ;))`;
        }
      }

      default:
        throw type.loc
          ? nodeErr(type, `unsupported type: ${type.type}`)
          : new Error(`unsupported type: ${type.type}`);
    }
  }

  function inBlock<T>(fn: () => T): T {
    const prevInstructions = instructions;
    const prevFuncs = funcs;
    const prevLocals = locals;
    const prevLocalTypes = localTypes;
    try {
      instructions = [];
      funcs = [];
      locals = [];
      localTypes = new Map<string, any>();
      return fn();
    } finally {
      instructions = prevInstructions;
      funcs = prevFuncs;
      locals = prevLocals;
      localTypes = prevLocalTypes;
    }
  }

  function generateLocal(prefix: string, type: string): string {
    const name = `$__${prefix}_${locals.length}__`;
    locals.push(`(local ${name} ${type})`);
    return name;
  }

  function resolveFunction(callee: any): string {
    if (callee.type !== "Identifier") {
      throw nodeErr(callee, `unsupported callee type: ${callee.type}`);
    }
    return `$${callee.name}`;
  }

  function nodeErr(node: any, msg: string) {
    return new LuauParser.SyntaxError(msg, "", node.loc);
  }

  function visit(node: any): unknown {
    switch (node.type) {
      case "Program":
        return inBlock(() => {
          node.body.forEach(visit);

          const imports = [
            '(func $print (import "luau:debug" "print") (param f64))',
          ];

          const sections = [...imports, ...types, ...funcs];

          if (instructions.length) {
            sections.push(
              `(func $$__main__$$
${[...locals, ...instructions].join("\n")}
)`,
              "(start $$__main__$$)"
            );
          }

          return `(module
${sections.join("\n\n")}
)
`;
        });

      case "TypeDecl": {
        function formatTypeDef(def: any): string {
          switch (def.type) {
            case "TableType": {
              if (def.props.length && def.indexes.length) {
                throw nodeErr(node, "Mixed table types are not supported");
              } else if (def.indexes.length > 1) {
                throw nodeErr(
                  node,
                  "Table types with multiple indexes are not supported"
                );
              }

              if (def.indexes.length) {
                return `(array)`;
              }

              const parts = [
                "struct",
                ...def.props.map((p: any) => {
                  return `(field $${p.key} (mut ${formatTypeDef(
                    p.valueType
                  )}))`;
                }),
              ];
              return `(${parts.join(" ")})`;
            }

            case "TypeRef": {
              switch (def.base) {
                case "number":
                  return "f64";

                default:
                  return `$${def.base}`;
              }
            }

            default: {
              console.error("unsupported type", def);
              return "f64";
            }
          }
        }
        types.push(`(type $${node.name} ${formatTypeDef(node.def)})`);
        return null;
      }

      case "FuncDecl": {
        const name = node.name.base;
        const params = (node.params ?? []).map((p: any) => {
          return `(param $${p.name} ${toWasmTypeRef(p.type)})`;
        });
        const returns = node.returnType.map((r: any) => {
          return `(result ${toWasmTypeRef(r)})`;
        });
        const signature = [
          `$${name}`,
          ...(node.local ? [] : [`(export "${name}")`]),
          ...params,
          ...returns,
        ].join(" ");
        const body = inBlock(() => {
          node.body.forEach(visit);
          return [...locals, ...instructions].join("\n");
        });
        funcs.push(`(func ${signature}
${body}
)`);
        return null;
      }

      case "VarDecl": {
        if (!node.local) {
          // Assume this is an assignment.
          node.exprs?.forEach((init: any) => {
            visit(init);
          });

          // Assign in reverse order
          [...node.vars].reverse().forEach((v: any, idx: number) => {
            // This sets a field!
            if (v.type === "PropertyAccess") {
              // We need to smuggle the object under the init value.
              const inferredPropType = "f64";
              locals.push(`(local $__propTemp__ ${inferredPropType})`);
              instructions.push(`local.set ${inferredPropType}`);
              visit(v.object);
              instructions.push(`local.get ${inferredPropType}`);
              const inferredTypeOfObject = "$Vector3D";
              instructions.push(`struct.set ${inferredTypeOfObject} $${v.key}`);
            } else {
              instructions.push(`local.set $${v.name}`);
            }
          });
          return null;
        }

        node.vars.forEach((v: any, idx: number) => {
          // Declare var
          locals.push(`(local $${v.name} ${toWasmTypeRef(v.type)})`);
          localTypes.set(v.name, v.type);
        });

        node.exprs?.forEach((init: any) => {
          visit(init);
        });

        if (node.exprs?.length) {
          [...node.vars].reverse().forEach((v: any, idx: number) => {
            instructions.push(`local.set $${v.name}`);
          });
        }
        return null;
      }

      case "TableLiteral": {
        const inferredType = "$Vector3D";
        instructions.push(`struct.new_default ${inferredType}`);
        if (node.fields.length) {
          // Generate unique name
          const localIdx = generateLocal(
            "struct",
            `(ref 0 (; ${inferredType} ;))`
          );
          instructions.push(`local.tee ${localIdx}`);

          node.fields.forEach((field: any, idx: number) => {
            const fieldIdx = field.key ? `$${field.key}` : idx;
            instructions.push(`local.get ${localIdx}`);
            visit(field.value);
            instructions.push(`struct.set ${inferredType} ${fieldIdx}`);
          });
        }
        return null;
      }

      case "PropertyAccess": {
        visit(node.object);
        const inferredTypeOfObject = "$Vector3D";
        instructions.push(`struct.get ${inferredTypeOfObject} $${node.key}`);
        return null;
      }

      case "BinaryExp": {
        visit(node.left);
        visit(node.right);
        switch (node.op) {
          case "+":
            instructions.push(`f64.add`);
            break;
          case "-":
            instructions.push(`f64.sub`);
            break;
          default:
            throw nodeErr(node, `unsupported binary exp: ${node.op}`);
        }
        return null;
      }

      case "CallStmt":
        // TODO: Clean up stack if the function returns anything.
        visit(node.expr);
        return null;

      case "CallExpression": {
        const { callee, method, args } = node;
        if (method) {
          throw nodeErr(node, "unsupported call w/ method");
        }
        const fnIdx = resolveFunction(callee);
        // Push args in order onto the stack.
        args.forEach(visit);
        instructions.push(`call ${fnIdx}`);
        return null;
      }

      case "Number": {
        instructions.push(`f64.const ${node.value}`);
        return null;
      }

      case "Return": {
        // Return is implicit by pushing the values onto the stack.
        node.expr.forEach(visit);
        return null;
      }

      case "Identifier": {
        // For now - assume these are all locals.
        instructions.push(`local.get $${node.name}`);
        return null;
      }

      default:
        console.error("unknown node type", node);
        return null;
    }
  }

  return visit(ast) as string;
}

interface WabtModule {
  toBinary(opts: unknown): { log: string; buffer: Uint8Array };
}

interface Wabt {
  parseWat(filename: string, wat: string, features: unknown): WabtModule;

  FEATURES: unknown;
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

function main(wabt: Wabt) {
  registerLanguages();

  const lastSource = window.localStorage.getItem("lastSource") ?? "print(2);\n";

  const srcEditor = createEditor("src", "lua", lastSource);
  const srcDecorations = srcEditor.createDecorationsCollection();

  const astEditor = createEditor("ast", "json");

  const watEditor = createEditor("wat", "wat");

  const runEditor = createEditor("run", "text");

  async function reparseSource() {
    const code = srcEditor.getValue();
    window.localStorage.setItem("lastSource", code);

    srcDecorations.clear();

    let ast;
    try {
      ast = LuauParser.parse(code);
      astEditor.setValue(JSON.stringify(ast, null, 2));
      monaco.editor.setModelMarkers(srcEditor.getModel()!, "luau", []);
    } catch (e) {
      if (isSyntaxError(e)) {
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
        astEditor.setValue(
          `At input.lua:${e.location.start.line}:${e.location.start.column} (${e.location.start.offset})\n\n${e.message}`
        );
        return;
      } else {
        throw e;
      }
    }

    let wat;
    try {
      wat = toWat(ast);
      watEditor.setValue(wat);
    } catch (e) {
      console.error(e);
      watEditor.setValue(`${e}`);
      if (isSyntaxError(e)) {
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

      const features = {
        ...(wabt.FEATURES as any),
        // tslint:disable-next-line:enforce-name-casing
        function_references: true,
        gc: true,
      };
      const parsedWat = wabt.parseWat("input.wat", wat, features);
      const binaryOutput = parsedWat.toBinary({
        log: true,
        // tslint:disable-next-line:enforce-name-casing
        write_debug_names: true,
      });

      await WebAssembly.instantiate(binaryOutput.buffer, {
        "luau:debug": {
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
}

const documentIsReady = /^loaded|^i|^c/.test(document.readyState)
  ? Promise.resolve()
  : new Promise<unknown>((resolve) => {
      document.addEventListener("DOMContentLoaded", resolve, { once: true });
      window.addEventListener("load", resolve, { once: true });
    });

declare var WabtModule: () => Promise<Wabt>;

Promise.all([WabtModule(), documentIsReady, loadMonaco()]).then(
  (args: [Wabt, ...unknown[]]) => {
    main(args[0]);
  }
);
