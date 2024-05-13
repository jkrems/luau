import { parse, SyntaxError as LuauSyntaxError } from "./luau.pegjs";

export { LuauSyntaxError };

export function isLuauSyntaxError(e: unknown): e is LuauSyntaxError {
  return e instanceof LuauSyntaxError;
}

export { parse };
