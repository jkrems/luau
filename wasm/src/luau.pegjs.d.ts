interface Location {
  offset: number;
  line: number;
  column: number;
}

interface Range {
  start: Location;
  end: Location;
}

export class SyntaxError extends Error {
  constructor(msg: string, expected: string, loc: Range);
}

export function parse(code: string): any;
