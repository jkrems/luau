interface CompilationResult {
  getOutput(): Uint8Array;
  delete();
}

interface LuauWeb {
  luauToWasm(code: string, optLevel: number, asWat: boolean): CompilationResult;
}

export default function loadModule(): Promise<LuauWeb>;
