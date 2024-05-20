interface LuauWeb {
  luauToWasm(code: string, optLevel: number, asWat: boolean): Uint8Array;
}

export default function loadModule(): Promise<LuauWeb>;
